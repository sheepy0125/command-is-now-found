/*
 * Command is now found CLI
 * Created by sheepy0125
 * 2022-09-02
 */

/***** Setup *****/
/* Imports */
use reqwest::{blocking::Client, redirect::Policy as RedirectPolicy};
use std::sync::{
    atomic::{AtomicBool, Ordering},
    Arc,
};
use thiserror::Error;

/* Enums */
/// Error
#[derive(Error, Debug)]
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
    /// Unknown error
    #[error("What? An unknown error occurred, sorry(!): {0}")]
    UnknownError(String),
}
impl From<reqwest::Error> for CommandWasError {
    fn from(e: reqwest::Error) -> CommandWasError {
        CommandWasError::RequestError(format!("{}", e))
    }
}

/// Distribution
enum Distribution {
    Arch,
    Debian,
    Fedora,
    Alpine,
    CentOS,
}

/// Settings
struct Settings {
    run_install_command: bool,
    find_command: bool,
    preferred_distribution: Distribution,
    verbose: bool,
}
impl Default for Settings {
    /// DEBUG default settings
    fn default() -> Settings {
        Settings {
            run_install_command: false,
            find_command: false,
            preferred_distribution: Distribution::Arch,
            verbose: true,
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
                let action = attempt.stop();
                redirected.store(true, Ordering::Relaxed);
                action
            })
        };
        // Handle the reqwest error as an unknown error here
        let client = match Client::builder().redirect(redirect_policy).build() {
            Ok(client) => client,
            Err(e) => {
                return Err(CommandWasError::UnknownError(format!(
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
                true => Err(CommandWasError::NoInformationError),
            },
            Err(e) => Err(CommandWasError::RequestError(format!("{}", e))),
        }
    }
}

fn main() {
    let settings = Settings::default();
    let scraper = Scraper::new(format!("rust"), settings);
    match scraper.get_html_response() {
        Ok(resp) => println!("resp {}", resp),
        Err(e) => println!("error {}", e),
    }
}
