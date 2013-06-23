(ert-deftest test-commander-not-greedy-when-single-required-argument ()
  (with-mock
   (mock (foo "bar") :times 1)
   (mock (baz) :times 1)
   (commander
    (option "--foo <bar>" "..." 'foo)
    (command "baz" "..." 'baz)
    (parse '("--foo" "bar" "baz")))))
