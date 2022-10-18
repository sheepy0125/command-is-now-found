# Command is now found

A simple-but-powerful tool to scrape [command-not-found.com](https://command-not-found.com) (which I did not make, see [Disclaimer](#disclaimer)) as the only public API available is for Alpine Linux.

## Features
- [x] Scrapes [command-not-found.com](https://command-not-found.com)  
- [x] Gives a summary of the commands for each distribution  
- [x] Can auto-determine the distribution to look for (opt-in)  
- [x] Can auto-install (opt-in)  

## Installation

You'll need [git](https://command-not-found.com/git) and [cargo](https://command-not-found.com/cargo) installed  .
Then, in a directory where you go to compile stuff (preferably `/opt` if your permissions are set up correctly but anywhere is fine):
```bash
git clone https://github.com/sheepy0125/command-is-now-found
cd command-is-now-found
cargo install --path .
```
Then, you could put something like this in your shell's rc file to make it easier:
```bash
alias how='command-is-now-found -i -r -c'
```
so you can use `how curl` for example.  
If you're going to do this, please check [Usage](#usage) and [Examples](#examples) for the flags.

## Usage

```
USAGE:
    command-is-now-found [OPTIONS]

OPTIONS:
    -c, --command <COMMAND>
            The command to find [default: ]

    -d, --distribution <PREFERRED_DISTRIBUTION>
            Distribution to get (if auto-find is disabled) [possible values: arch, debian, ubuntu,
            kali, raspbian, fedora, alpine, cent-os, windows, all]

    -h, --help
            Print help information

    -i, --find-preferred-distribution
            Automatically find the distribution to search for

    -n, --no-prepend
            Don't prepend anything before the commands

    -p, --prepend-before-commands <PREPEND_BEFORE_COMMANDS>
            What to prepend before the commands [default: sudo]

    -r, --run_install_command
            Automatically run the command that was found

        --verbose
            Verbose
```

## Examples

### Search for how to install curl for all distributions
`command-is-now-found -c curl`  
`command-is-now-found -d all -c curl`  
`command-is-now-found --distribution all --command curl`  
`command-is-now-found` and then entering `curl` when asked for a command

### Search for how to install curl for Arch Linux
`command-is-now-found -d arch -c curl`  
`command-is-now-found --distribution arch --command curl`

### Search for how to install curl for Alpine Linux using doas
`command-is-now-found -d alpine -p doas -c curl`  
`command-is-now-found --distribution alpine --prepend-before-commands doas --command curl`

### Search for how to install curl for your current distribution
`command-is-now-found -i -c curl`  
`command-is-now-found --find-preferred-distribution --command curl`

### Search for how to install curl for your current distribution, not modifying the command from the website at all
`command-in-now-found -i -n -c curl`  
`command-is-now-found --find-preferred-distribution --no-prepend --command curl`

### Search for how to install curl for your current distribution and run the command
`command-is-now-found -i -r -c curl`  
`command-is-now-found --find-preferred-distribution --run-install-command --command curl`

### Verbose mode
`command-is-now-found --verbose`  

## Did something break?
Please make an issue!  
Pull requests are accepted, just make an issue first if it's a non-trivial change.

## Changelog
```
1.0.0 / 2022-10-17
Initial release
```

## Disclaimer
I am not the owner of nor have any relation to [command-not-found.com](https://command-not-found.com) and do not condone sending a huge number of requests to it. I just use this as an easier method than switching to my web browser and I suggest you do only that as well. When making this, I downloaded the `curl` page from the website and used a web server to host it locally.  
This is licensed under the `MIT` license. See `COPYING` for details.
