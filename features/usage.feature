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

       --bar          BAR
       --foo          FOO
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

       bar          BAR
       foo          FOO
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

       bar            BAR
       foo            FOO

      OPTIONS:

       --bar          BAR
       --foo          FOO
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

       bar            BAR
       foo            FOO

      OPTIONS:

       --bar          BAR
       --foo          FOO
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

       bar            BAR
       foo            FOO
                      AND
                      FRIENDS

      OPTIONS:

       --bar          BAR
       --foo          FOO
                      AND
                      FRIENDS
      """
