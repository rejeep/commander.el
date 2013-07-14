(ert-deftest test-commander-not-greedy-when-single-required-argument ()
  (with-mock
   (mock (foo "bar") :times 1)
   (mock (baz) :times 1)
   (commander
    (option "--foo <bar>" "..." 'foo)
    (command "baz" "..." 'baz)
    (parse '("--foo" "bar" "baz")))))

(ert-deftest test-commander-skank ()
  (with-mock
   (mock (load "test/foo-bar.el") :times 1)
   (mock (run) :times 1)
   (commander
    (option "--load <*>" "..." 'load)
    (command "run [*]" "..." 'run)
    (parse '("run" "--load" "test/foo-bar.el")))))
