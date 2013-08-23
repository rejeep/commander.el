(ert-deftest commander-test/not-greedy-when-single-required-argument ()
  (with-mock
   (mock (foo "bar") :times 1)
   (mock (baz) :times 1)
   (commander
    (option "--foo <bar>" "..." foo)
    (command "baz" "..." baz)
    (parse ("--foo" "bar" "baz")))))

(ert-deftest commander-test/argument-kind-of-looking-like-argument ()
  (with-mock
   (mock (cat "test/foo-bar.el") :times 1)
   (mock (run) :times 1)
   (commander
    (option "--cat <*>" "..." cat)
    (command "run [*]" "..." run)
    (parse ("run" "--cat" "test/foo-bar.el")))))

(ert-deftest commander-test/not-calling-parse ()
  (with-mock
   (let ((command-line-args-left '("--" "--help")))
     (mock (help) :times 1)
     (commander
      (option "--help" "HELP" help)))))

(ert-deftest commander-test/unknown-directive ()
  (with-mock
   (mock (error "Unknown directive: %S" '(foo)))
   (commander
    (foo)
    (parse nil))))

(ert-deftest commander-test/with-default-function ()
  (with-mock
   (mock (no-command "foo" "bar" "baz" "qux"))
   (commander
    (option "-x <arg>" "..." ignore)
    (default no-command)
    (parse ("foo" "bar" "-x" "foo" "baz" "qux")))))

(ert-deftest commander-test/with-default-function-and-arguments ()
  (with-mock
   (mock (no-command "foo" "bar"))
   (commander
    (option "-x <arg>" "..." ignore)
    (default no-command "foo" "bar")
    (parse ("-x" "foo")))))

(ert-deftest commander-test/commander-args ()
  (with-mock
   (mock (foo) :times 1)
   (mock (bar) :times 1)
   (mock (baz) :times 1)
   (let ((commander-args '("--foo" "baz" "--bar")))
     (commander
      (option "--foo" "..." foo)
      (option "--bar" "..." bar)
      (command "baz" "..." baz)))))
