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
    process::{Command, Stdio},
    str::FromStr,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
};

use terminal_size::{terminal_size, Height, Width};

use question::{Answer, Question};

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
static DEFAULT_ANSWER_YES_FOR_RUNNING_COMMAND: bool = false;

/* Type aliases */
type InstallCommands = Vec<String>;

/* Enums */
/// Error
#[derive(Error, Debug, PartialEq)]
enum CommandWasError {
    /// Any request error
    #[error("There was an error with requesting: {0}")]
    RequestError(String),
    /// Any parse error
    #[error("There was an error with parsing: {0}")]
    ParseError(String),
    /// No information (redirected to homepage)
    #[error("There's no information for how to install that!")]
    NoInformationError,
    /// No information for the preferred distribution
    #[error("There's no information for your preferred distribution!")]
    NoInformationForSelectedDistributionError,
    /// Couldn't auto-detect something
    #[error("Couldn't auto-detect: {0}")]
    AutoDetectError(String),
    /// Any error with running the command
    #[error("Failed to auto run command: {0}")]
    AutoRunErrorCommand(String),
    /// Unknown error
    #[error("What? An unknown error occurred, sorry(!): {0}")]
    UnknownError(String),
    /// General error
    #[error("{0}")]
    GeneralError(String),
}
/// Implicitly convert reqwest errors
impl From<reqwest::Error> for CommandWasError {
    fn from(e: reqwest::Error) -> Self {
        CommandWasError::RequestError(format!("{}", e))
    }
}
/// Log errors
impl CommandWasError {
    fn log_error(&self, target: &str) {
        error!(target: target.to_lowercase().replace(' ', "_").as_str(), "{}", self);
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
    #[clap(
        short = 'c',
        long = "command",
        conflicts_with("find-command"),
        default_value = "",
        parse(try_from_str)
    )]
    command: String,

    /// What to prepend before the commands
    ///
    /// For example, most install commands will need to be ran as root with `sudo`
    /// `command-not-found.com` uses sudo rarely, and will be excluded before something
    /// gets prepended
    #[clap(
        short = 'p',
        long = "prepend-before-commands",
        default_value = "sudo",
        parse(try_from_str)
    )]
    prepend_before_commands: String,

    /// Don't prepend anything before the commands
    ///
    /// See `prepend-before-commands`, with this on the commands won't be modified
    #[clap(
        short = 'n',
        long = "no-prepend",
        conflicts_with = "prepend-before-commands",
        takes_value = false
    )]
    no_prepend: bool,

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
        conflicts_with = "command",
        takes_value = false
    )]
    find_command: bool,

    /// Automatically run the command that was found
    ///
    /// This will run the command for the preferred distribution gathered from the website
    ///
    /// Note: You will have to confirm the command before it can be ran
    #[clap(short = 'r', long = "run_install_command", takes_value = false)]
    run_install_command: bool,

    /// Automatically find the distribution to search for
    ///
    /// This will detect your distribution from `/etc/os-release`'s ID or ID_LIKE
    #[clap(short = 'i', long = "find-preferred-distribution", takes_value = false)]
    find_preferred_distribution: bool,

    /// Verbose
    ///
    /// This will show more (mostly debug) logs
    #[clap(long = "verbose", takes_value = false)]
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
struct InformationFinder;
impl InformationFinder {
    /// Create a new information finder
    fn new() -> Self {
        Self {}
    }

    /// Find the distribution automatically
    fn find_distribution(&self) -> Result<Distribution, CommandWasError> {
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

    /// Ask the user for the command. Will re-ask until a valid response is given
    fn ask_command(&self) -> String {
        let log_level = "asking user for command";

        match Question::new("Enter a command:").ask() {
            Some(Answer::RESPONSE(none)) if none.is_empty() => {
                GeneralError("Command cannot be empty".to_string()).log_error(log_level);
                self.ask_command()
            }
            Some(Answer::RESPONSE(command)) => command,
            _ => panic!("Response was not a response. How did this happen?"),
        }
    }
}

/***** Parser *****/
struct CommandParsed {
    is_preferred_distribution: bool,
    distribution: Distribution,
    install_commands: InstallCommands,
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
        let unpreferred_distribution_label_style =
            Style::new().fg(AnsiTermColor::Cyan).dimmed().bold();
        let preferred_distribution_label_style =
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
                .trim_start(),
            commands_value = (self
                .commands
                .iter()
                .map(|command_info| {
                    format!(
                        "{distribution_label}: {distribution}\n{commands}",
                        distribution_label = distribution_label_style.paint("Distribution"),
                        distribution = {
                            match command_info.is_preferred_distribution {
                                true => preferred_distribution_label_style
                                    .paint(command_info.distribution.to_string()),
                                false => unpreferred_distribution_label_style
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
    fn new(arguments: Arguments, distribution: Distribution) -> Self {
        Self {
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
                e.log_error("parsing card box");
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
                let distribution_selector = SelectorType::Command(distribution);
                let parsed_distribution_selector = match self.parse_selector(&distribution_selector)
                {
                    // Remember: return scope is map closure
                    Ok(parsed) if parsed.is_some() => parsed.unwrap(), // Some (unwrapping is safe)
                    Ok(_parsed) => return None,                        // None
                    Err(_) => {
                        GeneralError(format!("Error with selector {:?}!", selector_type))
                            .log_error(
                                format!("selector for distribution {}", distribution).as_str(),
                            );
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
                        if self.arguments.no_prepend {
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
                    .collect::<InstallCommands>();

                Some(CommandParsed {
                    is_preferred_distribution: (match self
                        .arguments
                        .preferred_distribution
                        .is_some()
                        || self.arguments.find_preferred_distribution
                    {
                        true => self.distribution == distribution,
                        false => false,
                    }),
                    distribution,
                    install_commands,
                })
            })
            .collect::<Vec<CommandParsed>>();

        match selector_type {
            SelectorType::Command(_) => Ok(Some(distributions)),
            _ => Err(CommandWasError::ParseError(
                "Didn't get a command selector for handling of a command selector".to_string(),
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
                            CommandWasError::NoInformationError.log_error(log_level);
                            ParseError(
                                "No information even though the page exists. \
                                Is the scraper broken?"
                                    .to_string(),
                            )
                            .log_error(log_level);
                        }
                        _ => (),
                    };
                    Vec::new()
                }
            },
            Err(e) => {
                e.log_error(log_level);
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

/* Command runner */
struct CommandRunner {
    parsed_response: ParsedResponse,
    commands: Option<InstallCommands>,
    command_string: Option<String>,
}
impl CommandRunner {
    fn new(parsed_response: ParsedResponse) -> Self {
        Self {
            parsed_response,
            command_string: None,
            commands: None,
        }
    }

    /// Get the install command from the parsed response from the preferred distribution
    /// If there is no preferred distribution, then it will ask for one
    fn get_install_command(
        &self,
        preferred_distribution: Option<Distribution>,
    ) -> Result<InstallCommands, CommandWasError> {
        match self.parsed_response.commands.iter().find(|command_parsed| {
            match preferred_distribution {
                Some(preferred_distribution) => {
                    command_parsed.distribution == preferred_distribution
                }
                None => command_parsed.is_preferred_distribution,
            }
        }) {
            Some(command) => Ok(command.install_commands.clone()),
            None => {
                // Do the same thing, but ask the user what distribution to use
                let preferred_distribution = match self.ask_preferred_distribution() {
                    Some(distribution) => {
                        // Ensure the distribution is valid and in the response
                        match self
                            .parsed_response
                            .commands
                            .iter()
                            .find(|command_parsed| command_parsed.distribution == distribution)
                        {
                            Some(_) => distribution,
                            None => {
                                return Err(
                                    CommandWasError::NoInformationForSelectedDistributionError,
                                )
                            }
                        }
                    }
                    None => {
                        return Err(CommandWasError::AutoRunErrorCommand(
                            "User did not give a preferred distribution".to_string(),
                        ))
                    }
                };
                self.get_install_command(Some(preferred_distribution))
            }
        }
    }

    /// Ask for a preferred distribution
    /// Will return None if the user declines
    /// Note: this is not in InformationFinder as it has specific stuff relating to validation
    fn ask_preferred_distribution(&self) -> Option<Distribution> {
        let sentinel_none = "none";
        let mut acceptable_answers = Vec::from(Distribution::ALL_DISTRIBUTIONS);
        acceptable_answers.push(sentinel_none);
        match Question::new(
            format!(
                "What distribution would you like to run the install command for? \
                Say \"{}\" to cancel:",
                sentinel_none
            )
            .as_str(),
        )
        .acceptable(acceptable_answers.clone())
        .clarification(
            format!(
                "That's not a possible distribution! Valid values: {}\n",
                acceptable_answers.join(", ")
            )
            .as_str(),
        )
        .until_acceptable()
        .ask()
        {
            Some(Answer::RESPONSE(none)) if none == sentinel_none => None,
            Some(Answer::RESPONSE(distribution)) => {
                Some(<Distribution as FromStr>::from_str(distribution.as_str()).unwrap())
                // safe to unwrap, it's acceptable
            }
            _ => {
                UnknownError("The distribution question response was not a response".to_string())
                    .log_error("asking preferred distribution");
                None
            }
        }
    }

    fn ask_should_run_command(&self) -> bool {
        match Question::new(
            format!(
                "Would you like to run the install command:\n{}",
                self.command_string.as_ref().unwrap() // Safe to unwrap, it's already been populated
            )
            .as_str(),
        )
        .yes_no()
        .until_acceptable()
        .default(match DEFAULT_ANSWER_YES_FOR_RUNNING_COMMAND {
            true => Answer::YES,
            false => Answer::NO,
        })
        .show_defaults()
        .clarification("Please either enter \"yes\" or \"no\"!\n")
        .ask()
        .unwrap() // Safe to unwrap, it'll keep asking and never be None
        {
            Answer::YES => true,
            Answer::NO => false,
            _ => {
                UnknownError("Value for question was not yes nor no, assuming no".to_string()).log_error(
                    "asking should run command"
                );
                false
            }
        }
    }

    fn run_install_command(&self) -> Result<(), CommandWasError> {
        match Command::new("sh")
            .arg("-c")
            .arg(self.command_string.as_ref().unwrap().as_str()) // safe to unwrap, not called when None
            .stdout(Stdio::inherit())
            .stdin(Stdio::inherit())
            .stderr(Stdio::inherit())
            .spawn()
        {
            Ok(_) => Ok(()),
            Err(e) => Err(AutoRunErrorCommand(e.to_string())),
        }
    }
}

fn run(arguments: &mut Arguments) -> Result<(), ()> {
    let mut arguments = arguments.clone();

    /* Find information */
    let information_finder = InformationFinder::new();

    // Find command
    info!("Finding command...");
    let command = match arguments.command.is_empty() {
        true => {
            println!();
            let command = information_finder.ask_command();
            arguments.command.push_str(command.as_str());
            command
        }
        false => arguments.command.clone(),
    };
    info!("Found command: {}", command);

    // Find distribution
    info!("Finding distribution...");
    let distribution = match arguments.find_preferred_distribution {
        true => match information_finder.find_distribution() {
            Ok(distribution) => distribution,
            Err(e) => {
                e.log_error("finding distribution");
                arguments.preferred_distribution.unwrap_or_default()
            }
        },
        false => arguments.preferred_distribution.unwrap_or_default(),
    };
    info!("Found distribution: {}", distribution);

    /* Scraper */
    let mut scraper = Scraper::new(arguments.clone(), distribution);

    // Get the HTML response
    info!("Getting HTML response...");
    match scraper.get_html_response() {
        Ok(()) => {
            trace!("Got response: {}", scraper.html);
        }
        Err(e) => {
            e.log_error("parser getting HTML response");
            return Err(());
        }
    };
    info!("Got HTML response");

    // Parse the HTML response
    info!("Parsing HTML response...");
    let parsed_response = match scraper.parse() {
        Ok(_) => {
            scraper.parsed_response.unwrap() // safe to unwrap, not error'd
        }
        Err(e) => {
            e.log_error("parsing");
            return Err(());
        }
    };
    info!("Information for {}", command);
    print!("\n{}\n", parsed_response);

    // Run the install commands
    if arguments.run_install_command {
        // We're allowed to, so let's do it
        let mut command_runner = CommandRunner::new(parsed_response);
        println!();
        match command_runner.get_install_command(None) {
            Ok(install_commands) => {
                command_runner.command_string = Some(install_commands.join("; "));
                command_runner.commands = Some(install_commands);
            }
            Err(e) => {
                e.log_error("getting install command");
                return Err(());
            }
        }
        if !command_runner.ask_should_run_command() {
            return Ok(());
        }
        match command_runner.run_install_command() {
            Ok(()) => (),
            Err(e) => {
                e.log_error("running install command");
                return Err(());
            }
        }
    }

    Ok(())
}

fn main() {
    // Get arguments
    let mut arguments = Arguments::parse();
    let verbose = arguments.verbose; // Will be moved in the logger format, don't use

    // Logger
    let columns = if let Some((Width(width), Height(_))) = terminal_size() {
        width
    } else {
        UnknownError("Terminal size width returned None".to_string())
            .log_error("getting terminal size");
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
                    "{potential_clear}\r{file}:{line} {time} : \
                    {log_level}{log_level_potential_padding} : \
                    {target} : \
                    {message}{potential_newline}",
                    // Do we need to clear for single status messages?
                    potential_clear = {
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
                    line = format_args!("{:0>4}", record.line().unwrap_or(0)),
                    time = Local::now().format("%Y-%m-%d %H:%M:%S"),
                    log_level = style.value(level),
                    log_level_potential_padding = match level {
                        Level::Info => " ",
                        _ => "",
                    },
                    target = record.target(),
                    message = line
                ) {
                    Ok(_) => {
                        buffer.flush()?;
                        Ok(())
                    }
                    Err(e) => Err(e),
                }
            }) {
                result?
            }
            Ok(())
        })
        .target(Target::Stderr) /* for removing of status message with 2>/dev/null
         *                                       as the output gets sent to stdout */
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
    match run(&mut arguments) {
        Ok(()) => (),
        Err(()) => GeneralError("Fatal error, exiting!".to_string()).log_error("run exited"),
    };
}
