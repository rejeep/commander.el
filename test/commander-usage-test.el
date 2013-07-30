(ert-deftest commander-test-usage/information ()
  (with-mock
   (commander
    (option "--foo <bar>" "..." ignore)
    (option "--help" "..." ignore)
    (command "baz <qux>" "..." ignore)
    (command "qux [*]" "..." ignore)
    (parse nil)
    (should
     (equal
      "USAGE: ert-runner COMMAND [OPTIONS]

COMMANDS:
 qux [*]             ...
 baz <qux>           ...

OPTIONS:
 --help              ...
 --foo <bar>         ...
"
      (commander-usage))))))

(ert-deftest commander-test-usage/with-custom-name ()
  (with-mock
   (commander
    (name "foo")
    (parse nil)
    (should
     (equal
      "USAGE: foo COMMAND [OPTIONS]

COMMANDS:


OPTIONS:

"
      (commander-usage))))))
