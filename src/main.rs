/*
 * Command is now found CLI
 * Created by sheepy0125
 * 2022-09-02
 */

/***** Setup *****/
/* Imports */
use thiserror::Error;

use std::{
    fmt::{Display, Formatter},
    io::Write,
    str::FromStr,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
};

use terminal_size::{terminal_size, Height, Width};

extern crate strfmt;
use std::collections::HashMap;
use strfmt::strfmt;

extern crate os_release;
use os_release::OsRelease;

#[macro_use]
extern crate log;
use ansi_term::{Color as AnsiTermColor, Style};
use chrono::Local;
use env_logger::{fmt::Color as EnvLoggerColor, Builder as LoggerBuilder, Env, Target};
use log::{Level, LevelFilter};

use clap::{ArgEnum, Parser as ClapParser};

use html_escape::decode_html_entities;
use scraper::{ElementRef, Html, Node::Text, Selector};

use reqwest::{blocking::Client, redirect::Policy as RedirectPolicy};

/* Statics */
static APP_USER_AGENT: &str = concat!(env!("CARGO_PKG_NAME"), "/", env!("CARGO_PKG_VERSION"));
static MIN_COLUMN_SINGLE_LINE_STATUS: u16 = 60;

/* Enums */
/// Error
#[derive(Error, Debug, PartialEq)]
enum CommandWasError {
    /// Request error
    #[error("There was an error with requesting: {0}")]
    RequestError(String),
    /// Any kind of parse error
    #[error("There was an error with parsing: {0}")]
    ParseError(String),
    /// No information (redirected to homepage)
    #[error("There's no information for how to install that!")]
    NoInformationError,
    /// No information for the preferred distribution
    /// Will show a hint to run with a flag
    #[error(
        "There's no information for your preferred distribution!\n\
        Hint: Run with `--distribution all` to get all distributions"
    )]
    NoInformationForSelectedDistributionError,
    /// Couldn't auto-detect something
    #[error("Couldn't auto-detect: {0}")]
    AutoDetectError(String),
    /// Was not allowed to auto-detect something
    #[error("Wasn't allowed to auto detect")]
    NotAllowedToAutoFind,
    /// Unknown error
    #[error("What? An unknown error occurred, sorry(!): {0}")]
    UnknownError(String),
}
/// Implicitly convert reqwest errors
impl From<reqwest::Error> for CommandWasError {
    fn from(e: reqwest::Error) -> CommandWasError {
        CommandWasError::RequestError(format!("{}", e))
    }
}
/// Log errors
impl CommandWasError {
    /// Errors to not log (in verbose mode they get logged)
    const NO_LOG_ERRORS: [CommandWasError; 1] = [CommandWasError::NotAllowedToAutoFind];

    /// Won't log errors that shouldn't be logged (in NO_LOG_ERRORS)
    fn log_error(level: &str, e: CommandWasError) {
        if !CommandWasError::NO_LOG_ERRORS.contains(&e) {
            error!(target: level, "{}", e);
            return;
        }
        // Not supposed to log, but the verbose users get special treatment
        debug!(target: level, "{}", e);
    }
}

use CommandWasError::*;

/// Distribution
/// There are actually more on the website, however, if they're from the same package
/// manager there's an almost certain chance they will be the same command
#[derive(Debug, ArgEnum, Clone, Copy, PartialEq)]
enum Distribution {
    Arch,
    Debian,
    Fedora,
    Alpine,
    CentOS,
    Windows,
    /// Will show information for all distributions
    All,
}
/// Default
impl Default for Distribution {
    fn default() -> Self {
        Self::All
    }
}
/// Displaying and converting to and from Strings
impl Distribution {
    /// Used for Clap
    const POSSIBLE_VALUES: &'static [&'static str] = &[
        "arch", "debian", "ubuntu", "kali", "raspbian", "fedora", "alpine", "centos", "windows",
    ];

    /// Used for iterating
    const ALL_DISTRIBUTIONS: &'static [&'static str] =
        &["arch", "debian", "fedora", "alpine", "centos", "windows"];
}
impl FromStr for Distribution {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        match input.to_lowercase().as_str() {
            "arch" => Ok(Self::Arch),
            "debian" => Ok(Self::Debian),
            "ubuntu" => Ok(Self::Debian),
            "kali" => Ok(Self::Debian),
            "raspbian" => Ok(Self::Debian),
            "fedora" => Ok(Self::Fedora),
            "alpine" => Ok(Self::Alpine),
            "centos" => Ok(Self::CentOS),
            "windows" => Ok(Self::Windows),
            _ => Err(()), // this will cause a panic?!
        }
    }
}
impl Display for Distribution {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{:#?}", self)
    }
}

/* Structs */

/// Arguments
#[derive(ClapParser, Clone)]
struct Arguments {
    /// The command to find (if auto-find is disabled)
    #[clap(short, short = 'c', long = "command", conflicts_with("find-command"))]
    command: String,

    /// What to prepend before the commands
    ///
    /// For example, most install commands will need to be ran as root with `sudo`
    /// `command-not-found.com` uses sudo rarely, and will be excluded before something
    /// gets prepended
    #[clap(
        short = 'p',
        long = "prepend-before-commands",
        parse(try_from_str),
        default_value = "sudo"
    )]
    prepend_before_commands: String,

    /// Don't prepend anything before the commands
    ///
    /// See `prepend-before-commands`, with this on the commands won't be modified
    #[clap(
        long = "should-prepend",
        // conflicts_with = "prepend-before-commands",
        parse(try_from_str),
        default_value = "true"
    )]
    should_prepend: bool,

    /// Distribution to get (if auto-find is disabled)
    ///
    /// Defaults to all distributions if not given
    #[clap(
        short = 'd',
        long = "distribution",
        possible_values = Distribution::POSSIBLE_VALUES,
        conflicts_with = "find-preferred-distribution",
        ignore_case = true
    )]
    #[clap(arg_enum)]
    preferred_distribution: Option<Distribution>,

    /// Automatically find the command to find
    ///
    /// This will detect the command from your shell history file (whatever command was
    /// ran before this program)
    #[clap(
        short = 'f',
        long = "find-command",
        parse(try_from_str),
        default_value = "false"
    )]
    find_command: bool,

    /// Automatically run the command that was found
    ///
    /// This will run the command for the preferred distribution gathered from the website
    ///
    /// Note: You will have to confirm the command before it can be ran
    #[clap(
        short = 'r',
        long = "run_install_command",
        parse(try_from_str),
        default_value = "false"
    )]
    run_install_command: bool,

    /// Automatically find the distribution to search for
    ///
    /// This will detect your distribution from `/etc/os-release`'s ID or ID_LIKE
    #[clap(
        long = "find-preferred-distribution",
        parse(try_from_str),
        default_value = "false"
    )]
    find_preferred_distribution: bool,

    /// Verbose
    ///
    /// This will show more (mostly debug) logs
    #[clap(long = "verbose", parse(try_from_str), default_value = "false")]
    verbose: bool,
}
/// Display (mostly for DEBUG)
impl Display for Arguments {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "\
            Arguments:\n \
            Command: {} \n \
            Run install command: {}\n \
            Find command: {}\n \
            Preferred distribution: {}\n \
            Find preferred distribution: {}\n \
            Verbose: {} \
            ",
            self.command,
            self.run_install_command,
            self.find_command,
            self.preferred_distribution.unwrap_or_default(),
            self.find_preferred_distribution,
            self.verbose
        )
    }
}

/***** Information finder *****/
struct InformationFinder {
    arguments: Arguments,
}
impl InformationFinder {
    /// Create a new information finder
    fn new(arguments: Arguments) -> Self {
        Self { arguments }
    }

    /// Find the distribution
    /// If disallowed in arguments, it won't auto-detect and instead use the preferred,
    /// it's okay to unwrap in this case!
    fn find_distribution(&self) -> Result<Distribution, CommandWasError> {
        // Are we allowed to?
        if !self.arguments.find_preferred_distribution {
            return Err(NotAllowedToAutoFind);
        }

        // We can find it from os-release's ID
        // See https://www.freedesktop.org/software/systemd/man/os-release.html
        let release = match OsRelease::new() {
            Ok(release) => release,
            Err(e) => return Err(AutoDetectError(format!("{}", e))),
        };
        let mut distribution = release.id_like;
        // Sometimes, the id_like won't be specified (this API will return an empty string in that case)
        // So, we'll need to use the other ID
        if (&distribution).is_empty() {
            distribution = release.id;
        }
        // In any case that that's also nonexistent (still empty!), raise an error
        if (&distribution).is_empty() {
            return Err(AutoDetectError(
                "os-release's id and id_like appear to be empty".to_string(),
            ));
        }

        match <Distribution as FromStr>::from_str(&distribution) {
            Ok(string_distribution) => Ok(string_distribution),
            Err(_) => Err(AutoDetectError(format!(
                "Distribution detected ({}) is not a valid distribution",
                &distribution
            ))),
        }
    }
}

/***** Parser *****/
struct CommandParsed {
    is_preferred_distribution: bool,
    distribution: Distribution,
    install_commands: Vec<String>,
}

#[derive(Default)]
struct ParsedResponse {
    maintainer: Option<String>,
    homepage: Option<String>,
    section: Option<String>,
    description: Option<String>,
    commands: Vec<CommandParsed>,
}
/// Display
impl Display for ParsedResponse {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let label_style = Style::new().fg(AnsiTermColor::White).bold();
        let distribution_label_style = Style::new().fg(AnsiTermColor::Purple);
        let unperferred_distribution_label_style =
            Style::new().fg(AnsiTermColor::Cyan).dimmed().bold();
        let perferred_distribution_label_style =
            Style::new().fg(AnsiTermColor::Cyan).bold().italic();

        write!(
            f,
            "{maintainer_label}\n{maintainer_value}\
            \n{homepage_label}\n{homepage_value}\
            \n{section_label}\n{section_value}\
            \n{description_label}\n{description_value}\
            \n\n{commands_value}",
            maintainer_label = label_style.paint("Maintainer"),
            maintainer_value = (self.maintainer.clone()).unwrap_or_else(|| "<none>".to_string()),
            homepage_label = label_style.paint("Homepage"),
            homepage_value = (self.homepage.clone()).unwrap_or_else(|| "<none>".to_string()),
            section_label = label_style.paint("Section"),
            section_value = (self.section.clone()).unwrap_or_else(|| "<none>".to_string()),
            description_label = label_style.paint("Description"),
            description_value = (self.description.clone())
                .unwrap_or_else(|| "<none>".to_string())
                .trim_start(), // TODO: should this be handled in the parser? ^
            commands_value = (self
                .commands
                .iter()
                .map(|command_info| {
                    format!(
                        "{distribution_label}: {distribution}\n{commands}",
                        distribution_label = distribution_label_style.paint("Distribution"),
                        distribution = {
                            match command_info.is_preferred_distribution {
                                true => perferred_distribution_label_style
                                    .paint(command_info.distribution.to_string()),
                                false => unperferred_distribution_label_style
                                    .paint(command_info.distribution.to_string()),
                            }
                        },
                        commands = command_info.install_commands.join("\n")
                    )
                })
                .collect::<Vec<String>>())
            .join("\n")
        )
    }
}

/// A selector type to get
#[derive(Debug, Clone)]
enum SelectorType {
    Maintainer,            // In the card box
    Homepage,              // In the card box
    Section,               // In the card box
    Description,           // Under the command name
    Command(Distribution), // In the card-body class
}
impl SelectorType {
    /***** Selector templates *****/
    /// A selector that's in a card-box. Specify nth_child depending on what card box link to get
    const BOX_SELECTOR: &'static str =
        ".card-command-meta>.list-group>.list-group-item:nth-child({nth_child})";
    /// The description selector
    const DESCRIPTION_SELECTOR: &'static str = ".row-command-info>div>p";
    /// Command selectors (we can't use the data-os attribute since it's not for all distributions)
    const COMMANDS_SELECTOR: &'static str = ".command-install.install-{distribution}>dd";

    fn str_format_hashmap_helper(key: &str, value: &str) -> HashMap<String, String> {
        let mut hashmap = HashMap::new();
        hashmap.insert(key.to_string(), value.to_string());
        hashmap
    }

    /// Get a string selector from an enum type
    fn get_selector(&self) -> String {
        match self {
            Self::Maintainer => strfmt(
                Self::BOX_SELECTOR,
                &Self::str_format_hashmap_helper("nth_child", "1"),
            )
            .unwrap(),
            Self::Homepage => format!(
                "{}>a",
                strfmt(
                    Self::BOX_SELECTOR,
                    &Self::str_format_hashmap_helper("nth_child", "2"),
                )
                .unwrap()
            ),
            Self::Section => strfmt(
                Self::BOX_SELECTOR,
                &Self::str_format_hashmap_helper("nth_child", "3"),
            )
            .unwrap(),
            Self::Description => Self::DESCRIPTION_SELECTOR.to_string(),
            Self::Command(distribution) => match distribution {
                Distribution::All => Self::COMMANDS_SELECTOR.to_string(),
                _ => strfmt(
                    Self::COMMANDS_SELECTOR,
                    &Self::str_format_hashmap_helper(
                        "distribution",
                        distribution.to_string().to_lowercase().as_str(),
                    ),
                )
                .unwrap(),
            },
        }
    }

    /// A boolean to tell if the thing we need to scrape is untagged text
    /// Some things we need to get are just wrapped in pure text (namely things in
    /// card-boxes that aren't links)
    /// CSS selectors are unable to get these, and hence here is a sorta-hacky
    /// solution to return a boolean if we need to get the pure text element!
    fn selector_needs_pure_text_without_tags(&self) -> bool {
        match self {
            Self::Maintainer => true,
            Self::Homepage => false,
            Self::Section => true,
            Self::Description => false,
            Self::Command(_) => panic!(
                "selector_needs_pure_text_without_tags shouldn't be called for \
                the command selector, it should be handled separately!"
            ),
        }
    }
}

/* Scraper */
struct Scraper {
    arguments: Arguments,
    distribution: Distribution,
    html: String,
    parsed_html: Html,
    parsed_response: Option<ParsedResponse>,
}
impl Scraper {
    fn new(arguments: Arguments, distribution: Distribution) -> Scraper {
        Scraper {
            arguments,
            distribution,
            html: String::new(),
            parsed_html: Html::new_document(),
            parsed_response: None,
        }
    }

    /// Get the HTML response
    fn get_html_response(&mut self) -> Result<(), CommandWasError> {
        // We want to catch redirects, which signify no information
        let redirected = Arc::new(AtomicBool::new(false));
        let redirect_policy = {
            let redirected = redirected.clone();
            RedirectPolicy::custom(move |attempt| {
                redirected.store(true, Ordering::Relaxed);
                attempt.stop()
            })
        };
        // Handle the reqwest error as an unknown error here
        let client = match Client::builder()
            .redirect(redirect_policy)
            .user_agent(APP_USER_AGENT)
            .build()
        {
            Ok(client) => client,
            Err(e) => {
                return Err(UnknownError(format!(
                    "Could not make request client: {}",
                    e
                )));
            }
        };

        self.html = match client
            .get(format!("http://localhost:8090/{}", self.arguments.command))
            .send()?
            .text()
        {
            Ok(text) => match redirected.load(Ordering::Relaxed) {
                false => text,
                true => return Err(NoInformationError),
            },
            Err(e) => return Err(RequestError(format!("{}", e))),
        };

        Ok(())
    }

    /// Parse everything
    fn parse(&mut self) -> Result<(), CommandWasError> {
        let mut parsed_response = ParsedResponse::default();

        self.parsed_html = match Html::parse_document(&self.html) {
            // Detect if there's an error
            parsed if !parsed.errors.is_empty() => {
                return Err(CommandWasError::ParseError(format!(
                    "Error with parsing tree: {}",
                    parsed.errors.join(", ")
                )));
            }
            parsed => parsed,
        };

        {
            use SelectorType::*;
            parsed_response.homepage = self.get_card_box_selector_wrapper(Homepage);
            parsed_response.description = self.get_card_box_selector_wrapper(Description);
            parsed_response.maintainer = self.get_card_box_selector_wrapper(Maintainer);
            parsed_response.section = self.get_card_box_selector_wrapper(Section);
            parsed_response.commands =
                match self.get_command_selector_wrapper(Command(self.distribution)) {
                    commands if commands.is_empty() => {
                        return Err(CommandWasError::NoInformationForSelectedDistributionError)
                    }
                    commands => commands,
                }
        }

        self.parsed_response = Some(parsed_response);

        Ok(())
    }

    /// Get card box selector
    fn get_card_box_selector(
        &self,
        selector_type: SelectorType,
    ) -> Result<Option<String>, CommandWasError> {
        let selector_parsed_element = match self.parse_selector(&selector_type) {
            Ok(optional_element) => match optional_element {
                Some(parsed_element) => parsed_element,
                None => {
                    return Err(CommandWasError::ParseError(
                        "Couldn't find a card box selector!".to_string(),
                    ));
                }
            },
            Err(e) => {
                return Err(e);
            }
        };

        // Sometimes, we need to get text that's not wrapped in any HTML tags.
        if selector_type.selector_needs_pure_text_without_tags() {
            let collected_texts = selector_parsed_element
                .children()
                .filter_map(|child| match child.value() {
                    Text(text) => {
                        // Remove unneeded characters
                        let text_string = text
                            .to_string()
                            .replace(&['\n', '<', '>'], "")
                            .trim()
                            .to_string();

                        // If it's empty, just don't include it when collecting
                        match text_string.is_empty() {
                            true => None,
                            false => Some(text_string),
                        }
                    }
                    _ => None,
                })
                .collect::<Vec<String>>();

            match collected_texts.len() {
                0 => Ok(None),
                _ => Ok(Some(collected_texts.join("\n"))),
            }
        } else {
            // If we don't need to do that, we can just return the first inner HTML we get
            Ok(Some(format!(
                "{}",
                // &lt; and &gt; sometimes exist
                decode_html_entities(&selector_parsed_element.inner_html())
            )))
        }
    }
    /// Wrapper for getting a card box selector with error handling
    fn get_card_box_selector_wrapper(&self, selector: SelectorType) -> Option<String> {
        match self.get_card_box_selector(selector) {
            Ok(result) => result,
            Err(e) => {
                CommandWasError::log_error("parsing card box", e);
                None
            }
        }
    }

    /// Get command selector
    fn get_command_selector(
        &self,
        selector_type: SelectorType,
    ) -> Result<Option<Vec<CommandParsed>>, CommandWasError> {
        // Get commands for every distribution
        let distributions = Distribution::ALL_DISTRIBUTIONS
            .iter()
            .map(|distribution| <Distribution as FromStr>::from_str(distribution).unwrap())
            .filter_map(|distribution| {
                // Attempt to find the command from the HTML
                // Sometimes, it'll just not be there
                // Attempt to find the command from the HTML
                // Sometimes, it'll just not be there
                let distribution_selector = SelectorType::Command(distribution);
                let parsed_distribution_selector = match self.parse_selector(&distribution_selector)
                {
                    // Remember: return scope is map closure
                    Ok(parsed) if parsed.is_some() => parsed.unwrap(), // Some (unwrapping is safe)
                    Ok(_parsed) => return None,                        // None
                    Err(_) => {
                        error!("Error with selector {:?}!", selector_type);
                        return None; // skip distribution
                    }
                };

                // Now, find the commands!
                let install_commands = parsed_distribution_selector
                    .text()
                    // Strip newlines
                    .filter_map(|text| match text {
                        text if text == "\n" => None,
                        text => Some(text),
                    })
                    // Convert to Strings
                    .map(|text_str| text_str.to_string())
                    // Prepend to commands
                    .map(|text| {
                        // Should we?
                        if !self.arguments.should_prepend {
                            // We shalln't!
                            return text;
                        }
                        // Remove `sudo` if it exists (this isn't the best way, as if a
                        // package manager started with any part of `sudo` then it would
                        // get removed)
                        let mut text = match text.strip_prefix("sudo ") {
                            Some(text) => text.to_string(),
                            None => text.clone(), // the text passed in to the map
                        };
                        // Add the prepend string
                        text.insert_str(
                            0,
                            format!("{} ", self.arguments.prepend_before_commands).as_str(),
                        );
                        text
                    })
                    .collect::<Vec<String>>();

                Some(CommandParsed {
                    is_preferred_distribution: (match self.arguments.preferred_distribution {
                        Some(preferred_distribution) => preferred_distribution == distribution,
                        None => false,
                    }),
                    distribution,
                    install_commands,
                })
            })
            .collect::<Vec<CommandParsed>>();

        match selector_type {
            SelectorType::Command(distribution) => {
                use Distribution::*;
                match distribution {
                    // If the distribution is all distributions, great!
                    // We can return what we have
                    All => Ok(Some(distributions)),
                    // Otherwise, we have to find the distribution that we need
                    _ => {
                        let found = distributions
                            .iter()
                            .find(|command_parsed| command_parsed.distribution == distribution);

                        match found {
                            Some(found) => {
                                // For some reason, I couldn't use .to_owned() to get an owned
                                // struct. Take this hacky workaround instead! (please fix XXX)
                                let found_owned = CommandParsed {
                                    is_preferred_distribution: found.is_preferred_distribution,
                                    distribution: found.distribution,
                                    install_commands: found.install_commands.clone(),
                                };
                                Ok(Some(vec![found_owned]))
                            }
                            None => Ok(None),
                        }
                    }
                }
            }
            _ => Err(CommandWasError::ParseError(
                "Didn't get a comnmand selector for handling of a command selector".to_string(),
            )),
        }
    }
    /// Wrapper for getting a card box selector with error handling
    fn get_command_selector_wrapper(&self, selector: SelectorType) -> Vec<CommandParsed> {
        let log_level = "parsing commands";

        match self.get_command_selector(selector.clone()) {
            Ok(result) => match result {
                Some(result) => result,
                None => {
                    // So, we didn't get the response we needed. Because we know the page
                    // existed from the redirect handler, this is the selected distribution
                    // being nonexistent

                    match selector {
                        // Sanity check (could happen when the scraper breaks)
                        SelectorType::Command(distribution)
                            if distribution == Distribution::All =>
                        {
                            CommandWasError::log_error(
                                log_level,
                                CommandWasError::NoInformationError,
                            );
                            CommandWasError::log_error(
                                log_level,
                                CommandWasError::ParseError(
                                    "No information even though the page exists. \
                                    Is the scraper broken?"
                                        .to_string(),
                                ),
                            );
                        }
                        _ => (),
                    };
                    Vec::new()
                }
            },
            Err(e) => {
                CommandWasError::log_error(log_level, e);
                Vec::new()
            }
        }
    }

    /// Parse a selector and return the first element gotten
    fn parse_selector(
        &self,
        selector_type: &SelectorType,
    ) -> Result<Option<ElementRef>, CommandWasError> {
        let parsed_selector = match Selector::parse(&selector_type.get_selector()) {
            Ok(parsed_selector) => parsed_selector,
            Err(e) => {
                return Err(CommandWasError::ParseError(format!(
                    "Error with selector {:?}: {:?}",
                    selector_type, e,
                )))
            }
        };
        debug!("Selector for {:?}: {:?}", selector_type, parsed_selector);

        // Parse the selector
        // Just get the first
        let selector_parsed = self.parsed_html.select(&parsed_selector).next();
        // If we have nothing from the selector, we have nothing to parse
        match selector_parsed {
            Some(selector_parsed) => Ok(Some(selector_parsed)),
            None => Ok(None),
        }
    }
}

fn run(arguments: Arguments) {
    // Find information
    // Note: If we aren't allowed to find the information, it won't look for it
    // and we can just set it to a fallback
    info!("Finding information...");
    let information_finder = InformationFinder::new(arguments.clone());
    let distribution = match information_finder.find_distribution() {
        Ok(distribution) => distribution,
        Err(e) => {
            CommandWasError::log_error("finding distribution", e);
            arguments.preferred_distribution.unwrap_or_default()
        }
    };
    info!("Found distribution: {}", distribution);

    // Scrape
    let mut scraper = Scraper::new(arguments.clone(), distribution);

    // Get the HTML response
    info!("Getting HTML response...");
    match scraper.get_html_response() {
        Ok(()) => {
            trace!("Got response: {}", scraper.html);
        }
        Err(e) => {
            CommandWasError::log_error("parser getting HTML response", e);
            return;
        }
    };
    info!("Got HTML response");

    // Parse the HTML response
    info!("Parsing HTML response...");
    match scraper.parse() {
        Ok(_) => {
            info!("Information for {}", arguments.command);
            println!("\n{}", scraper.parsed_response.unwrap()) // safe to unwrap, not error'd
        }
        Err(e) => CommandWasError::log_error("parsing", e),
    }
}

fn main() {
    // Get arguments
    let arguments = Arguments::parse();
    let verbose = arguments.verbose; // Will be moved in the logger format, don't use

    // Logger
    let columns = if let Some((Width(width), Height(_))) = terminal_size() {
        width
    } else {
        CommandWasError::log_error(
            "getting terminal size",
            UnknownError("Terminal size width returned None".to_string()),
        );
        80
    };

    LoggerBuilder::from_env(Env::new().default_write_style_or("always"))
        // Verbose mode for this module if needed
        .filter_module(
            "command_is_now_found_cli",
            match arguments.verbose {
                true => LevelFilter::Debug,
                false => LevelFilter::Info,
            },
        )
        // Don't have verbose on for any other module
        .filter(None, LevelFilter::Info)
        // Formatting
        .format(move |buffer, record| {
            let level = record.level();
            let mut style = buffer.style();
            match record.level() {
                Level::Error => style.set_color(EnvLoggerColor::Red),
                Level::Info => style.set_color(EnvLoggerColor::Green),
                Level::Warn => style.set_color(EnvLoggerColor::Yellow),
                Level::Debug => style.set_color(EnvLoggerColor::Blue),
                _ => style.set_color(EnvLoggerColor::Cyan),
            };
            style.set_bold(true);

            // Split the arguments by newline, so we can prepend the text for every line
            // If an error occurs, it'll only catch the first one, but then it'll stop execution
            // of other write!'s
            for result in record.args().to_string().split('\n').map(|line| {
                // If verbose mode is deactivated and an info message is sent, then only show that on a
                // single line and have that line change. However, if the terminal columns is too few,
                // then do it normally again
                let single_line_status = matches!(verbose, false if (
                        record.level() == Level::Info &&
                        columns > MIN_COLUMN_SINGLE_LINE_STATUS
                    ));
                match write!(
                    buffer,
                    "{potential_clear}\r{file}:{line} {time} [{log_level}] : {message}{potential_newline}",
                    // Do we need to clear for single status messages?
                    potential_clear={
                        match single_line_status {
                            // Should be \r'd already
                            true => " ".to_string().repeat(columns.into()),
                            false => "".to_string(),
                        }
                    },
                    // Should we \r for single-line status messages or \n?
                    potential_newline = {
                        match single_line_status {
                            true => "\r".to_string(),
                            false => "\n".to_string(),
                        }
                    },
                    file = record.file().unwrap_or("unknown"),
                    line = record.line().unwrap_or(0),
                    time = Local::now().format("%Y-%m-%d %H:%M:%S"),
                    log_level = style.value(level),
                    message = line
                ) {
                    Ok(_) => {
                        buffer.flush()?;
                        Ok(())
                    },
                    Err(e) => Err(e),
                }
            }) {
                result?
            }
            Ok(())
        })
        .target(Target::Stderr) // for removing of status message with 2>/dev/null
        .init();

    debug!(
        "Number of columns: {}, exceeds minimum for single line statussing \
        (not in verbose) {}: {}",
        columns,
        MIN_COLUMN_SINGLE_LINE_STATUS,
        (columns > MIN_COLUMN_SINGLE_LINE_STATUS)
    );

    debug!("{}", arguments);

    info!("Running");
    run(arguments);
}
