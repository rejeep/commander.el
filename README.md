# commander.el [![Build Status](https://api.travis-ci.org/rejeep/commander.el.png?branch=master)](http://travis-ci.org/rejeep/commander.el)

Command line parsing for Emacs.

## Installation

I recommend installing via ELPA, but manual installation is simple as well:

    (add-to-list 'load-path "/path/to/commander")
    (require 'commander)

## DSL

### Overview

* [commander](https://github.com/rejeep/commander.el#commander-rest-forms) `(&rest forms)`
* [command](https://github.com/rejeep/commander.el#command-command-description-function-optional-default-value) `(command description function &optional default-value)`
* [option](https://github.com/rejeep/commander.el#option-flags-description-function-optional-default-value) `(flags description function &optional default-value)`
* [name](https://github.com/rejeep/commander.el#name-name) `(name)`
* [default](https://github.com/rejeep/commander.el#default-command-rest-arguments) `(command &rest arguments)`
* [parse](https://github.com/rejeep/commander.el#parse-arguments) `(arguments)`

### Details

#### commander `(&rest forms)`

Define schema within this block.

    (commander
     ;; schema here
     )

#### command `(command description function &optional default-value)`

Define a command.

##### Example

Define the command `foo` with no arguments.

    (commander
     (command "foo" "Foo" 'fn))

##### Usage

    $ emacs -Q -- foo

##### Example

Define the command `foo` with a required argument.

    (commander
     (command "foo <bar>" "Foo" 'fn))

##### Usage

     $ emacs -Q -- foo bar

##### Example

Define the command `foo` with an optional argument. If argument is not
specified, `"baz"` will be used as the argument value.

    (commander
     (command "foo [bar]" "Foo" 'fn "baz"))

##### Usage

     $ emacs -Q -- foo
     $ emacs -Q -- foo qux

##### Example

Define the command `foo` with at least one required argument.

    (commander
     (command "foo <*>" "Foo" 'fn))

##### Usage

     $ emacs -Q -- foo bar
     $ emacs -Q -- foo bar baz qux

##### Example

Define the command `foo` with zero or more arguments.

    (commander
     (command "foo [*]" "Foo" 'fn))

##### Usage

     $ emacs -Q -- foo
     $ emacs -Q -- foo bar baz qux

#### option `(flags description function &optional default-value)`

Define an option.

##### Example

Define the option `--foo` with no arguments.

    (commander
     (option "--foo" "Foo" 'fn))

##### Usage

    $ emacs -Q -- --foo

##### Example

Define the option `--foo` with a required argument.

    (commander
     (command "--foo <bar>" "Foo" 'fn))

##### Usage

    $ emacs -Q -- --foo bar

##### Example

Define the option `--foo` with an optional argument. If argument is not
specified, `"baz"` will be used as the argument value.

    (commander
     (command "--foo [bar]" "Foo" 'fn "baz"))

##### Usage

    $ emacs -Q -- --foo
    $ emacs -Q -- --foo qux

##### Example

Define the option `--foo` with at least one required argument.

    (commander
     (command "--foo <*>" "Foo" 'fn))

##### Usage

    $ emacs -Q -- --foo bar
    $ emacs -Q -- --foo bar baz qux

##### Example

Define the option `--foo` with zero or more arguments.

    (commander
     (command "--foo [*]" "Foo" 'fn))

##### Usage

    $ emacs -Q -- --foo
    $ emacs -Q -- --foo bar baz qux

##### Example

Define the option `--foo` with with an alias `-f`.

    (commander
     (option "--foo, -f" "Foo" 'fn))

##### Usage

    $ emacs -Q -- --foo
    $ emacs -Q -- -f

#### name `(name)`

Specify name in usage information.

##### Example

Define the option `--help` that prints usage information with
`my-awesome-program` as program name.

    (commander
     (name "my-awesome-program")
     (option "--help" "Show usage information" 'commander-print-usage))

##### Usage

    $ emacs -Q -- --help

#### default `(command &rest arguments)`

Specify default command if no command is specified.

##### Example

Define two commands `show` and `hide` and make `show` the default with
`everyone` as argument.

    (commander
     (default "show" "everyone")
     (command "show <stuff>" "Show stuff" 'show)
     (command "hide <stuff>" "Hide stuff" 'hide))

##### Usage

    $ emacs -Q -- show me
    $ emacs -Q -- hide you
    $ emacs -Q

#### parse `(arguments)`

Parse `arguments` with defined schema. If `#parse` is not called
manually, it is done automatically with `(cdr
command-line-args-left)` as value.

    (commander
     ;; schema

     (parse some-custom-variable)
     )

## Examples

Create a new project, with optional dev mode:

    ;; emacs -Q -- create
    ;; emacs -Q -- create --dev
    (commander
     (command "create" "Create new project" 'create)
     (option "--dev" "Run command in dev mode" 'dev-mode))

Simple find task:

    ;; emacs -Q -- find
    ;; emacs -Q -- find path/to/dir --name 'foo.el' --type f
    (commander
     (command "find [path]" "Find stuff" 'find)
     (option "--name <path>" "Specify file name" 'name)
     (option "--type <type>" "Specify file type" 'name))

Automatic usage information:

    (commander
     (command "find [path]" "Find stuff" 'find)
     (command "help" "Show usage information" 'commander-print-usage)
     (option "--name <path>" "Specify file name" 'name)
     (option "--type <type>" "Specify file type" 'name))

The command `emacs -Q -- help` will print:

    USAGE: find COMMAND [OPTIONS]

    COMMANDS:
     help                Show usage information
     find                Find stuff

    OPTIONS:
     --type              Specify file type
     --name              Specify file name

For more examples, check out: https://github.com/rejeep/commander.el/tree/master/examples

## Contribution

Contribution is much welcome!

Install [carton](https://github.com/rejeep/carton) if you haven't
already, then:

    $ cd /path/to/commander.el
    $ carton

Run all tests with:

    $ make
