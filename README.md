# WIP

# Command is now found

A simple-but-powerful tool to scrape [command-not-found.com](https://command-not-found.com) (which I did not make) as there's no public API available.

## Features / road-map
- [x] Scrapes [command-not-found.com](https://command-not-found.com)

- [x] Gives a summary of the commands for each distribution if specified, or a specific distribution

- [x] Can auto-determine the distribution to look for (opt-in)

- [ ] Can read the last entry of `.zsh_history` and `.bash_history` and auto-search (opt-in)

- [x] Can auto-install (opt-in)

## Installation

You'll need [git](https://command-not-found.com/git) and [cargo](https://command-not-found.com/cargo) installed

```bash
# You should go to a directory like `/opt` if you set the permissions up right
git clone https://github.com/sheepy0125/command-is-now-found
cd command-is-now-found
cargo install
```

## Did something break?

Please make an issue!

Pull requests are accepted, just make an issue first if it's a non-trivial change.

# TODO: usage lol