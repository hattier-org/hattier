# Hattier

A Haskell formatter focused on vertical alignment and configurability. Written as part of a group assignment for the _Advanced Functional Programming_ course at Utrecht University.

# Build Instructions

The project should be easily buildable via `cabal build`.  
If that is not the case, please ensure you have GHC 9.10.3 installed.  
GHC versions can easily be installed and managed by [GHCup](https://www.haskell.org/ghcup).  
Alternatively, Nix can also be used as apt configuration files are present in the repository.

# Project Directory Structure

```
hattier/
├── app/Main.hs                  # The command-line interface.
├── src/                         # The Hattier library's source code.
│   ├── Hattier.hs               # The top-level module, exporting all of Hattier's functionality.
│   └── Hattier/                 # The top-level directory, containing the key modules.
│       ├── Config.hs            # The fully-generic configuration.
│       ├── Format.hs            # Plain module, which exposes the top-level formatting function.
│       ├── Parser.hs            # Responsible for parsing Haskell source code via GHC's own parser.
│       ├── Types.hs             # Hattier's monad stack.
│       └── Printer/             # All core formatting and printing functionality, logically separated into individual modules.
│           └── [..]             # Formatting modules, separated logically into small composable pieces.
├── test/                        # Hattier's test suite
│   ├── Main.hs                  # Top-level module, that runs all tests
│   ├── Unit/                    # Small per-module unit tests for individual functionality
│   ├── Integration/             # Integration tests on the whole application
│   └── Property/                # Property-based tests with quickcheck
├── data/examples/               # Small examples to let you quickly test out Hattier!
├── README.md                    # The file you are reading right now!
├── hattier.dhall                # A sample configuration file we use ourselves at Hattier.
├── hattier.cabal                # The cabal-install specification for this package.
├── cabal.project                # Project-specific options we use at Hattier.
└── [.env|flake.nix|flake.lock]  # Nix-relevant files.
```


# Contributing

> [!IMPORTANT]
> Every change to the codebase should be done through pull
> requests, even if they will be self-approved.

## Strategy

Modular design:

- We start with the *primary* features that we want (like
  indentation) so that `hattier` can be run after another
  formatter like [ormolu](https://github.com/tweag/ormolu)
  or
  [stylish-haskell](https://github.com/haskell/stylish-haskell).
- After that, we can implement *core* features to make
  `hattier` self-sufficient.

## Branch structure

Where possible, please use a test-driven approach in your pr's. 
This makes reviewing far easier and helps in modularity.
To do this, try and structure your branches as follows:

- A commit writing tests, causing the automated tests to fail
- A commit implementing the feature, causing the automated tests (one of which you added in the previous commit!) to pass
- The inevitable "I forgot a semicolon" commit

This structures allows people reviewing your pr to look at the test first, which makes it far easier to understand your code!

## Commit messages

We'll adopt the following commit message syntax:

- **primary**: primary feature (see [Strategy](#strategy))
- **core**: core feature (see [Strategy](#strategy))
- **feat**: some other feature
- **fix**: fix some issue
- **test**: implemented one ore more tests
- **refactor**: code refactoring
- **docs**: updating documentation like the README or
  [Haddock](https://haskell-haddock.readthedocs.io/latest/)
  comments
- **chore**: simple tasks like updating a package
- **ci**: a change to the CI/CD pipeline

## Making tickets

We'll use GitHub Issues to make tickets and track who is
working on a particular feature. This consists mainly of two
parts:

1. Make issue with a title like "Ticket: implementing
   alignment of case expressions"
2. Assign yourself to work on a ticket by adding yourself
   to the assignees of the relevant issue.
