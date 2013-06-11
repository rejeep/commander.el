(ert-deftest test-commander-usage-information ()
  (with-mock
   (commander
    (option "--foo <bar>" "..." 'ignore)
    (option "--help" "..." 'ignore)
    (command "baz <qux>" "..." 'ignore)
    (command "qux [*]" "..." 'ignore)
    (parse '("--foo" "bar" "baz" "hax"))
    (should
     (equal
      "USAGE: commander-testrunner.el COMMAND [OPTIONS]

COMMANDS:
 qux [*]             ...
 baz <qux>           ...

OPTIONS:
 --help              ...
 --foo <bar>         ...
"
      (commander-usage))))))

(ert-deftest test-commander-usage-with-custom-name ()
  (with-mock
   (commander
    (name "foo")
    (should
     (equal
      "USAGE: foo COMMAND [OPTIONS]

COMMANDS:


OPTIONS:

"
      (commander-usage))))))
