Feature: Usage

  Scenario: Name
    Given this schedule:
      """
      (commander
       (name "command"))
      """
    Then usage should be:
      """
      USAGE: command [COMMAND] [OPTIONS]
      """

  Scenario: Name and description
    Given this schedule:
      """
      (commander
       (name "command")
       (description "COMMAND"))
      """
    Then usage should be:
      """
      USAGE: command [COMMAND] [OPTIONS]

      COMMAND
      """

  Scenario: Name and option
    Given this schedule:
      """
      (commander
       (name "command")
       (option "--foo" "FOO" ignore))
      """
    Then usage should be:
      """
      USAGE: command [COMMAND] [OPTIONS]

      OPTIONS:

       --foo          FOO
      """

  Scenario: Name and options
    Given this schedule:
      """
      (commander
       (name "command")
       (option "--foo" "FOO" ignore)
       (option "--bar" "BAR" ignore))
      """
    Then usage should be:
      """
      USAGE: command [COMMAND] [OPTIONS]

      OPTIONS:

       --foo          FOO
       --bar          BAR
      """

  Scenario: Name and command
    Given this schedule:
      """
      (commander
       (name "command")
       (command "foo" "FOO" ignore))
      """
    Then usage should be:
      """
      USAGE: command [COMMAND] [OPTIONS]

      COMMANDS:

       foo          FOO
      """

  Scenario: Name and commands
    Given this schedule:
      """
      (commander
       (name "command")
       (command "foo" "FOO" ignore)
       (command "bar" "BAR" ignore))
      """
    Then usage should be:
      """
      USAGE: command [COMMAND] [OPTIONS]

      COMMANDS:

       foo          FOO
       bar          BAR
      """

  Scenario: Name, options and commands
    Given this schedule:
      """
      (commander
       (name "command")
       (command "foo" "FOO" ignore)
       (command "bar" "BAR" ignore)
       (option "--foo" "FOO" ignore)
       (option "--bar" "BAR" ignore))
      """
    Then usage should be:
      """
      USAGE: command [COMMAND] [OPTIONS]

      COMMANDS:

       foo            FOO
       bar            BAR

      OPTIONS:

       --foo          FOO
       --bar          BAR
      """

  Scenario: Name, description, options and commands
    Given this schedule:
      """
      (commander
       (name "command")
       (description "COMMAND")
       (command "foo" "FOO" ignore)
       (command "bar" "BAR" ignore)
       (option "--foo" "FOO" ignore)
       (option "--bar" "BAR" ignore))
      """
    Then usage should be:
      """
      USAGE: command [COMMAND] [OPTIONS]

      COMMAND

      COMMANDS:

       foo            FOO
       bar            BAR

      OPTIONS:

       --foo          FOO
       --bar          BAR
      """

  Scenario: Multiline descriptions
    Given this schedule:
      """
      (commander
       (name "command")
       (command "foo" ("FOO" "AND" "FRIENDS") ignore)
       (command "bar" "BAR" ignore)
       (option "--foo" ("FOO" "AND" "FRIENDS") ignore)
       (option "--bar" "BAR" ignore))
      """
    Then usage should be:
      """
      USAGE: command [COMMAND] [OPTIONS]

      COMMANDS:

       foo            FOO
                      AND
                      FRIENDS
       bar            BAR

      OPTIONS:

       --foo          FOO
                      AND
                      FRIENDS
       --bar          BAR
      """

  Scenario: Auto docs
    Given this schedule:
      """
      (commander
       (name "command")

       (command "foo" foo)
       (command "bar" bar 1 2 3)

       (option "--baz" baz)
       (option "--qux" "Qux" qux 1 2 3))
      """
    Then usage should be:
      """
      USAGE: command [COMMAND] [OPTIONS]

      COMMANDS:

       foo            Return FOO.
       bar            Print BAR.
                      
                      More info.
                      And more info.

      OPTIONS:

       --baz          
       --qux          Qux
      """

  Scenario: Uniq options
    Given this schedule:
      """
      (commander
       (name "command")

       (command "baz <qux>" "BAZ" ignore)
       (command "foo" "FOO" ignore)
       (command "bar" "BAR" ignore)

       (option "--bar, --baz, -q" "BAR, BAZ and q" ignore)
       (option "--foo" "foo" ignore))
      """
    Then usage should be:
      """
      USAGE: command [COMMAND] [OPTIONS]

      COMMANDS:

       baz <qux>                 BAZ
       foo                       FOO
       bar                       BAR

      OPTIONS:

       --bar, --baz, -q          BAR, BAZ and q
       --foo                     foo
      """
