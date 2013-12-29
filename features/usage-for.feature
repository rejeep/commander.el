Feature: Usage for

  Scenario: Single row
    Given this schedule:
      """
      (commander
       (name "command")
       (command "foo" "This command does foo." ignore))
      """
    Then usage for "foo" should be:
      """
      This command does foo.
      """

  Scenario: Multiple rows
    Given this schedule:
      """
      (commander
       (name "command")
       (command "foo" ("This command does foo." "And bar.") ignore))
      """
    Then usage for "foo" should be:
      """
      This command does foo.
      And bar.
      """

  Scenario: Command does not exist
    Given this schedule:
      """
      (commander
       (name "command"))
      """
    Then usage for "foo" should error
