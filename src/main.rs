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
    str::FromStr,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
};

extern crate os_release;
use os_release::OsRelease;

#[macro_use]
extern crate log;
extern crate pretty_env_logger;
use env_logger::{Builder as LoggerBuilder, Target};
use log::LevelFilter;

use clap::{ArgEnum, Parser};

use reqwest::{blocking::Client, redirect::Policy as RedirectPolicy};

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
        Hint: Run with --all-distributions to get all distributions"
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
#[derive(Debug, ArgEnum, Clone, Copy)]
enum Distribution {
    Arch,
    Debian,
    Fedora,
    Alpine,
    CentOS,
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
    /// Used for Clap (probably not the best way of doing it)
    const POSSIBLE_VALUES: &'static [&'static str] =
        &["arch", "debian", "fedora", "alpine", "centos"];
}
impl FromStr for Distribution {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        match input {
            "arch" => Ok(Self::Arch),
            "debian" => Ok(Self::Debian),
            "fedora" => Ok(Self::Fedora),
            "alpine" => Ok(Self::Alpine),
            "centos" => Ok(Self::CentOS),
            _ => Err(()),
        }
    }
}
impl Display for Distribution {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{:#?}", self)
    }
}

/// Arguments
#[derive(Parser, Clone)]
struct Arguments {
    /// The command to find (if auto-find is disabled)
    #[clap(short, short = 'c', long = "command", conflicts_with("find-command"))]
    command: String,

    /// Preferred distribution (if auto-find is disabled)
    ///
    /// Defaults to all distributions if not given
    #[clap(
        short = 'd',
        long = "preferred-distribution",
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
    #[clap(short = 'f', long = "find-command", default_value = "false")]
    find_command: bool,

    /// Automatically run the command that was found
    ///
    /// This will run the command for the preferred distribution gathered from the website
    ///
    /// Note: You will have to confirm the command before it can be ran
    #[clap(short = 'r', long = "run_install_command", default_value = "false")]
    run_install_command: bool,

    /// Automatically find the distribution to search for
    ///
    /// This will detect your distribution from `/etc/os-release`'s ID or ID_LIKE
    #[clap(long = "find-preferred-distribution", default_value = "false")]
    find_preferred_distribution: bool,

    /// Verbose
    ///
    /// This will show more (mostly debug) logs
    #[clap(long = "verbose", default_value = "false")]
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
        Self {
            arguments: arguments,
        }
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
            return Err(AutoDetectError(format!(
                "os-release's id and id_like appear to be empty"
            )));
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

/***** Scraper *****/
struct Scraper {
    arguments: Arguments,
}
impl Scraper {
    /// Create a new scraper
    fn new(arguments: Arguments) -> Self {
        Self {
            arguments: arguments,
        }
    }

    /// Get the HTML response
    fn get_html_response(&self) -> Result<String, CommandWasError> {
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
        let client = match Client::builder().redirect(redirect_policy).build() {
            Ok(client) => client,
            Err(e) => {
                return Err(UnknownError(format!(
                    "Could not make request client: {}",
                    e
                )));
            }
        };

        match client
            .get(format!(
                "https://command-not-found.com/{}",
                self.arguments.command
            ))
            .send()?
            .text()
        {
            Ok(text) => match redirected.load(Ordering::Relaxed) {
                false => Ok(text),
                true => Err(NoInformationError),
            },
            Err(e) => Err(RequestError(format!("{}", e))),
        }
    }
}

fn run(arguments: Arguments) {
    // Find information
    // Note: If we aren't allowed to find the information, it won't look for it
    // and we can just set it to a fallback
    let information_finder = InformationFinder::new(arguments.clone());
    let distribution = match information_finder.find_distribution() {
        Ok(distribution) => distribution,
        Err(e) => {
            CommandWasError::log_error("finding distribution", e);
            arguments.clone().preferred_distribution.unwrap_or_default()
        }
    };
    debug!("Found distribution: {}", distribution);

    // Get the HTML response
    // let scraper = Scraper::new(arguments);
    // let resp = match scraper.get_html_response() {
    // Ok(resp) => resp,
    // Err(e) => {
    // CommandWasError::log_error("scraper", e);
    // return;
    // }
    // };
    // debug!("Response: {}", resp)
}

fn main() {
    // Get arguments
    let arguments = Arguments::parse();

    // Logger
    LoggerBuilder::from_default_env()
        .format_target(true)
        .filter(
            None,
            match arguments.verbose {
                true => LevelFilter::Debug,
                false => LevelFilter::Info,
            },
        )
        .target(Target::Stdout)
        .init();

    debug!("{}", arguments);

    run(arguments);
}
