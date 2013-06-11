# commander.el [![Build Status](https://api.travis-ci.org/rejeep/commander.el.png?branch=master)](http://travis-ci.org/rejeep/commander.el)

Command line parsing for Emacs

## Installation

I recommend installing via ELPA, but manual installation is simple as well:

    (add-to-list 'load-path "/path/to/commander")
    (require 'commander)

## DSL

#### commander `(&rest forms)`

Define command/option schema within this block.

    (commander
     ;; schema here
     )

#### command `(command description function &optional default-value)`

Define a command. Examples:

    (commander
     ;; no arguments
     ;;  emacs -Q -- foo
     (command "foo" "Foo" 'fn)

     ;; one required argument
     ;;  emacs -Q -- foo bar
     (command "foo <bar>" "Foo" 'fn)

     ;; one optional argument
     ;;  emacs -Q -- foo
     ;;  emacs -Q -- foo baz
     (command "foo [bar]" "Foo" 'fn "bar")

     ;; one or more arguments
     ;;  emacs -Q -- foo bar
     ;;  emacs -Q -- foo bar baz qux
     (command "foo <*>" "Foo" 'fn)

     ;; zero or more arguments
     ;;  emacs -Q -- foo
     ;;  emacs -Q -- foo bar baz qux
     (command "foo [*]" "Foo" 'fn)
    )

#### option `(flags description function &optional default-value)`

Define an option. Examples:

    (commander
     ;; no arguments
     ;;  emacs -Q -- --foo
     (option "--foo" "Foo" 'fn)

     ;; one required argument
     ;;  emacs -Q -- --foo bar
     (command "--foo <bar>" "Foo" 'fn)

     ;; one optional argument
     ;;  emacs -Q -- --foo
     ;;  emacs -Q -- --foo baz
     (command "--foo [bar]" "Foo" 'fn "bar")

     ;; one or more arguments
     ;;  emacs -Q -- --foo bar
     ;;  emacs -Q -- --foo bar baz qux
     (command "--foo <*>" "Foo" 'fn)

     ;; zero or more arguments
     ;;  emacs -Q -- --foo
     ;;  emacs -Q -- --foo bar baz qux
     (command "--foo [*]" "Foo" 'fn)

     ;; option alias
     ;;  emacs -Q -- --foo
     ;;  emacs -Q -- -f
     (option "--foo, -f" "Foo" 'fn)
    )

#### name `(name)`

Specify name in usage information.

    ;;  emacs -Q -- --help
    (commander
     (name "my-awesome-program")
     (option "--help" "Show usage information" 'commander-print-usage))

#### default `(command &rest arguments)`

Specify default command if no command is specified.

    ;;  emacs -Q -- foo
    ;;  emacs -Q
    (commander
     (default "show" "stuffing")
     (command "show <stuff>" "Show stuff" 'show)
     (command "hide <stuff>" "Hide stuff" 'hide))

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
     (option "--dev" "Create in dev mode" 'dev-mode))

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
