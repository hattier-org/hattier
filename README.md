# LaLeXeR

We format stuff.

# Strategy

Modular design:

- We start with the *primary* features that we want (like
  indentation) so that `lalexer` can be run after another
  formatter like [ormolu](https://github.com/tweag/ormolu)
  or
  [stylish-haskell](https://github.com/haskell/stylish-haskell).
- After that, we can implement *core* features to make
  LaLeXeR self-sufficient.

# Contributing

> [!IMPORTANT]
> Every change to the codebase should be done through pull
> requests, even if they will be self-approved.

## Commit messages

We'll adopt the following commit message syntax:

- **primary**: primary feature (see [Strategy](#strategy))
- **core**: core feature (see [Strategy](#strategy))
- **feat**: some other feature
- **fix**: fix some issue
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
2. Assign yourself to work on a ticket by leaving a comment
   to the issue like "Assigned to vicgeentor"
