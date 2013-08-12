(ert-deftest commander-test-usage/with-name ()
  (with-mock
   (commander
    (name "foo")
    (parse nil))
   (should
    (equal
     "USAGE: foo [COMMAND] [OPTIONS]"
     (commander-usage)))))

(ert-deftest commander-test-usage/with-description ()
  (with-mock
   (commander
    (name "foo")
    (description "FOO BAR")
    (parse nil))
   (should
    (equal
     "USAGE: foo [COMMAND] [OPTIONS]

FOO BAR"
     (commander-usage)))))

(ert-deftest commander-test-usage/everything ()
  (with-mock
   (commander
    (name "foo")
    (description "FOO BAR")
    (option "--foo <bar>" "..." ignore)
    (option "--help" "..." ignore)
    (command "baz <qux>" "..." ignore)
    (command "qux [*]" "..." ignore)
    (parse nil))
   (should
    (equal
     "USAGE: foo [COMMAND] [OPTIONS]

FOO BAR

COMMANDS:

 qux [*]             ...
 baz <qux>           ...

OPTIONS:

 --help              ...
 --foo <bar>         ..."
     (commander-usage)))))
