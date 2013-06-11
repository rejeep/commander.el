(ert-deftest test-commander-both-options-and-commands ()
  (with-mock
   (mock (x) :times 1)
   (mock (help "bar") :times 1)
   (mock (show "qux") :times 1)
   (mock (command) :times 1)
   (commander
    (option "-x" "X" 'x)
    (option "--show <command>" "SHOW" 'show)
    (option "--help [command]" "HELP" 'help)
    (command "command" "COMMAND" 'command)
    (parse '("-x" "--help" "bar" "--show" "qux" "command")))))

(ert-deftest test-commander-not-greedy-when-single-required-argument ()
  (with-mock
   (mock (foo "bar") :times 1)
   (mock (baz) :times 1)
   (commander
    (option "--foo <bar>" "..." 'foo)
    (command "baz" "..." 'baz)
    (parse '("--foo" "bar" "baz")))))
