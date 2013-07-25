(ert-deftest test-commander-not-greedy-when-single-required-argument ()
  (with-mock
   (mock (foo "bar") :times 1)
   (mock (baz) :times 1)
   (commander
    (option "--foo <bar>" "..." foo)
    (command "baz" "..." baz)
    (parse ("--foo" "bar" "baz")))))

(ert-deftest test-commander-argument-kind-of-looking-like-argument ()
  (with-mock
   (mock (cat "test/foo-bar.el") :times 1)
   (mock (run) :times 1)
   (commander
    (option "--cat <*>" "..." cat)
    (command "run [*]" "..." run)
    (parse ("run" "--cat" "test/foo-bar.el")))))

(ert-deftest test-commander-not-calling-parse ()
  (with-mock
   (let ((command-line-args-left '("--" "--help")))
     (mock (help) :times 1)
     (commander
      (option "--help" "HELP" help)))))
