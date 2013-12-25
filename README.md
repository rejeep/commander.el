# commander.el [![Build Status](https://api.travis-ci.org/rejeep/commander.el.png?branch=master)](http://travis-ci.org/rejeep/commander.el)

Command line parsing for Emacs.

## Installation

Add `commander` to your [Cask](https://github.com/cask/cask) file:

```lisp
(depends-on "commander")
```

## DSL

### Overview

* [commander](https://github.com/rejeep/commander.el#commander-rest-forms) `(&rest forms)`
* [command](https://github.com/rejeep/commander.el#command-rest-args) `(&rest args)`
* [option](https://github.com/rejeep/commander.el#option-rest-args) `(&rest args)`
* [name](https://github.com/rejeep/commander.el#name-name) `(name)`
* [description](https://github.com/rejeep/commander.el#description-description) `(description)`
* [default](https://github.com/rejeep/commander.el#default-command-or-function-rest-arguments) `(command-or-function &rest arguments)`
* [parse](https://github.com/rejeep/commander.el#parse-arguments) `(arguments)`

### Details

#### commander `(&rest forms)`

Define schema within this block.

```lisp
(commander
 ;; schema here
 )
```

#### command `(&rest args)`

Define a command.

* First argument is the name of the command.
* Second argument either:
  * a description of the command
  * the command function (the function doc-string will be used as description)
* Third argument is command function if description is specified
* Rest of `args` are command default values

##### Example

Define the command `foo` with no arguments.

```lisp
(commander
 (command "foo" "Foo" fn))
```

##### Usage

```sh
$ emacs -Q -- foo
```

##### Example

Define the command `foo` with a required argument.

```lisp
(commander
 (command "foo <bar>" "Foo" fn))
```

##### Usage

```sh
$ emacs -Q -- foo bar
```

##### Example

Define the command `foo` with an optional argument. If argument is not
specified, `"baz"` will be used as the argument value.

```lisp
(commander
 (command "foo [bar]" "Foo" fn "baz"))
```

##### Usage

```sh
$ emacs -Q -- foo
$ emacs -Q -- foo qux
```

##### Example

Define the command `foo` with at least one required argument.

```lisp
(commander
 (command "foo <*>" "Foo" fn))
```

##### Usage

```sh
$ emacs -Q -- foo bar
$ emacs -Q -- foo bar baz qux
```

##### Example

Define the command `foo` with zero or more arguments.

```lisp
(commander
 (command "foo [*]" "Foo" fn))
```

##### Usage

```sh
$ emacs -Q -- foo
$ emacs -Q -- foo bar baz qux
```

##### Example

Define the command `foo` without description. The `foo` doc-string
will be used as description.

```lisp
(defun foo ()
  "Return FOO.")

(commander
 (default commander-print-usage-and-exit)
 (command "foo" foo))
```

##### Usage

```sh
$ emacs -Q
```

#### option `(&rest args)`

Define an option.

* First argument is the flags.
* Second argument either:
  * a description of the option
  * the option function (the function doc-string will be used as description)
* Third argument is option function if description is specified
* Rest of `args` are option default values

##### Example

Define the option `--foo` with no arguments.

```lisp
(commander
 (option "--foo" "Foo" fn))
```

##### Usage

```sh
$ emacs -Q -- --foo
```

##### Example

Define the option `--foo` with a required argument.

```lisp
(commander
 (command "--foo <bar>" "Foo" fn))
```

##### Usage

```sh
$ emacs -Q -- --foo bar
```

##### Example

Define the option `--foo` with an optional argument. If argument is not
specified, `"baz"` will be used as the argument value.

```lisp
(commander
 (command "--foo [bar]" "Foo" fn "baz"))
```

##### Usage

```sh
$ emacs -Q -- --foo
$ emacs -Q -- --foo qux
```

##### Example

Define the option `--foo` with at least one required argument.

```lisp
(commander
 (command "--foo <*>" "Foo" fn))
```

##### Usage

```sh
$ emacs -Q -- --foo bar
$ emacs -Q -- --foo bar baz qux
```

##### Example

Define the option `--foo` with zero or more arguments.

```lisp
(commander
 (command "--foo [*]" "Foo" fn))
```

##### Usage

```sh
$ emacs -Q -- --foo
$ emacs -Q -- --foo bar baz qux
```

##### Example

Define the option `--foo` with with an alias `-f`.

```lisp
(commander
 (option "--foo, -f" "Foo" fn))
```

##### Usage

```sh
$ emacs -Q -- --foo
$ emacs -Q -- -f
```

##### Example

Define the option `--foo` without description. The `foo` doc-string
will be used as description.

```lisp
(defun foo ()
  "Return FOO.")

(commander
 (default commander-print-usage-and-exit)
 (option "--foo" foo))
```

##### Usage

```sh
$ emacs -Q
```

#### name `(name)`

Specify name in usage information.

##### Example

Define the option `--help` that prints usage information with
`my-awesome-program` as program name.

```lisp
(commander
 (name "my-awesome-program")
 (option "--help" "Show usage information" commander-print-usage))
```

##### Usage

```sh
$ emacs -Q -- --help
```

#### description `(description)`

Specify description in usage information.

##### Example

Define the option `--help` that prints usage information with description.

```lisp
(commander
 (name "my-awesome-program")
 (description "Truly awesome program, does what you wish for")
 (option "--help" "Show usage information" commander-print-usage))
```

##### Usage

```sh
$ emacs -Q -- --help
```

#### default `(command-or-function &rest arguments)`

Specify default behaviour when no matching commands.

If `command-or-function` is a string, use that command if no command
is specified. If symbol, call that function with all arguments if
first argument does not match a command.

##### Example

Define two commands `show` and `hide` and make `show` the default with
`everyone` as argument.

```lisp
(commander
 (default "show" "everyone")
 (command "show <stuff>" "Show stuff" show)
 (command "hide <stuff>" "Hide stuff" hide))
```

##### Usage

```sh
$ emacs -Q -- show me
$ emacs -Q -- hide you
$ emacs -Q
```

##### Example

For each file argument, print the content.

```lisp
(defun print-file-content (file)
  (princ (f-read file)))

(commander
 (default print-file-content "foo.txt"))
```

##### Usage

```sh
$ emacs -Q -- foo.txt bar.txt
$ emacs -Q
```

#### config `(file)`

Parse `file` for default arguments and command. Each line is a command
or option, including arguments.

##### Example

For each file argument, print the content.

```lisp
;; cmd.opts
;;
;;  --foo bar

(commander
 (config "cmd.opts")
 (option "--foo <arg>" "..." ignore)
 (option "--bar <arg>" "..." ignore))
```

##### Usage

```sh
$ emacs -Q -- --bar arg
$ emacs -Q
```

#### parse `(arguments)`

Parse `arguments` with defined schema. If `#parse` is not called
explicitly, it is done automatically with `commander-args` first and
if that's not present, it is called with the value of `(cdr
command-line-args-left)`.

```lisp
(commander
 ;; schema

 (parse some-custom-variable)
 )
```

## Examples

Create a new project, with optional dev mode:

```lisp
;; emacs -Q -- create
;; emacs -Q -- create --dev
(commander
 (command "create" "Create new project" create)
 (option "--dev" "Run command in dev mode" dev-mode))
```

Simple find task:

```lisp
;; emacs -Q -- find
;; emacs -Q -- find path/to/dir --name 'foo.el' --type f
(commander
 (command "find [path]" "Find stuff" find)
 (option "--name <path>" "Specify file name" name)
 (option "--type <type>" "Specify file type" type))
```

Automatic usage information (note that if description is not
specified, the function doc-string is used as description):

```lisp
(commander
 (command "find [path]" "Find stuff" find)
 (command "help" "Show usage information" commander-print-usage)
 (option "--name <path>" "Specify file name" name)
 (option "--type <type>" "Specify file type" type))
```

The command `emacs -Q -- help` will print:

```
USAGE: find COMMAND [OPTIONS]

COMMANDS:
 helpShow usage information
 findFind stuff

OPTIONS:
 --type  Specify file type
 --name  Specify file name
```

For more examples, check out: https://github.com/rejeep/commander.el/tree/master/examples

## Contribution

Contribution is much welcome!

Install [cask](https://github.com/cask/cask) if you haven't
already, then:

```sh
$ cd /path/to/commander.el
$ cask
```

Run all tests with:

```sh
$ make
```
