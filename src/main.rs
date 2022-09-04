/*
 * Command is now found CLI
 * Created by sheepy0125
 * 2022-09-02
 */

/***** Setup *****/
/* Imports */
extern crate os_release;
use clap::Command;
extern crate pretty_env_logger;
#[macro_use]
extern crate log;
use env_logger::{Builder as LoggerBuilder, Target};
use log::LevelFilter;
use os_release::OsRelease;
use reqwest::{blocking::Client, redirect::Policy as RedirectPolicy};
use std::{
    fmt::{Display, Formatter},
    io::Write,
    str::FromStr,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
};
use thiserror::Error;

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
    /// I promise this won't be as bad (soon!)
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
#[derive(Debug, Clone, Copy)]
enum Distribution {
    Arch,
    Debian,
    Fedora,
    Alpine,
    CentOS,
    /// Will show information for all distributions
    All,
}
/// Displaying and converting to and from Strings
impl FromStr for Distribution {
    type Err = ();

    fn from_str(input: &str) -> Result<Distribution, Self::Err> {
        use Distribution::*;
        match input {
            "arch" => Ok(Arch),
            "debian" => Ok(Debian),
            "fedora" => Ok(Fedora),
            "alpine" => Ok(Alpine),
            "centos" => Ok(CentOS),
            _ => Err(()),
        }
    }
}
impl Display for Distribution {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{:#?}", self)
    }
}

/// Settings
#[derive(Clone, Copy)]
struct Settings {
    run_install_command: bool,
    find_command: bool,
    preferred_distribution: Distribution,
    find_preferred_distribution: bool,
    verbose: bool,
}
impl Default for Settings {
    /// DEBUG default settings
    fn default() -> Settings {
        Settings {
            run_install_command: false,
            find_command: false,
            preferred_distribution: Distribution::All,
            find_preferred_distribution: true,
            verbose: true,
        }
    }
}
impl Display for Settings {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "\
            Settings:\n \
            Run install command: {}\n \
            Find command: {}\n \
            Preferred distribution: {}\n \
            Find preferred distribution: {}\n \
            Verbose: {}\n \
            ",
            self.run_install_command,
            self.find_command,
            self.preferred_distribution,
            self.find_preferred_distribution,
            self.verbose
        )
    }
}

/***** Information finder *****/
struct InformationFinder {
    settings: Settings,
}
impl InformationFinder {
    /// Create a new information finder
    fn new(settings: Settings) -> InformationFinder {
        InformationFinder { settings: settings }
    }

    /// Find the distribution
    /// If disallowed in settings, it won't auto-detect and instead use the preferred,
    /// it's okay to unwrap in this case!
    fn find_distribution(&self) -> Result<Distribution, CommandWasError> {
        // Are we allowed to?
        if !self.settings.find_preferred_distribution {
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

        match Distribution::from_str(&distribution) {
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
    command: String,
    settings: Settings,
}
impl Scraper {
    /// Create a new scraper
    fn new(command: String, settings: Settings) -> Scraper {
        Scraper {
            command: command,
            settings: settings,
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
            .get(format!("https://command-not-found.com/{}", self.command))
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

fn run(settings: Settings) {
    // Find information
    // Note: If we aren't allowed to find the information, it won't look for it
    // and we can just set it to a fallback
    let information_finder = InformationFinder::new(settings);
    let distribution = match information_finder.find_distribution() {
        Ok(distribution) => distribution,
        Err(e) => {
            CommandWasError::log_error("finding distribution", e);
            settings.preferred_distribution
        }
    };
    debug!("Found distribution: {}", distribution);

    // let scraper = Scraper::new(format!("rust"), settings);
    // let resp = match scraper.get_html_response() {
    //     Ok(resp) => resp,
    //     Err(e) => {
    //         CommandWasError::log_error("scraper", e);
    //         return;
    //     }
    // };
}

fn main() {
    // Get settings
    let settings = Settings::default();

    // Logger
    LoggerBuilder::from_default_env()
        .format_target(true)
        .filter(
            None,
            match settings.verbose {
                true => LevelFilter::Debug,
                false => LevelFilter::Info,
            },
        )
        .target(Target::Stdout)
        .init();

    debug!("{}", settings);

    run(Settings::default());
}
