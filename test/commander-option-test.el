(ert-deftest test-commander-parse-expression ()
  (let ((arguments '("--help")))
    (with-mock
     (mock (help) :times 1)
     (commander
      (option "--help" "HELP" 'help)
      (parse arguments)))))

(ert-deftest test-commander-not-calling-parse ()
  (with-mock
   (let ((command-line-args-left '("--" "--help")))
     (mock (help) :times 1)
     (commander
      (option "--help" "HELP" 'help)))))

(ert-deftest test-commander-option-present-long ()
  (with-mock
   (mock (help) :times 1)
   (commander
    (option "--help" "HELP" 'help)
    (parse '("--help")))))

(ert-deftest test-commander-option-present-short ()
  (with-mock
   (mock (help) :times 1)
   (commander
    (option "-h" "HELP" 'help)
    (parse '("-h")))))

(ert-deftest test-commander-option-present-with-others ()
  (with-mock
   (mock (help) :times 1)
   (commander
    (option "--before" "BEFORE" 'ignore)
    (option "--after" "AFTER" 'ignore)
    (option "--help" "HELP" 'help)
    (parse '("--before" "--help" "--after")))))

(ert-deftest test-commander-option-alias ()
  (with-mock
   (mock (help) :times 2)
   (commander
    (option "--help, -h" "HELP" 'help)
    (parse '("--help"))
    (parse '("-h")))))

(ert-deftest test-commander-option-none ()
  (with-mock
   (not-called help)
   (commander
    (option "--help" "HELP" 'help)
    (parse nil))))

(ert-deftest test-commander-option-not-present ()
  (with-mock
   (mock (error "Option `%s` not available" "--do-not-help"))
   (commander
    (option "--help" "HELP" 'help)
    (parse '("--do-not-help")))))

(ert-deftest test-commander-option-required-argument-present ()
  (with-mock
   (mock (help "command") :times 1)
   (commander
    (option "--help <command>" "COMMAND HELP" 'help)
    (parse '("--help" "command")))))

(ert-deftest test-commander-option-required-argument-present-with-friends ()
  (with-mock
   (mock (help "command") :times 1)
   (commander
    (option "--help <command>" "COMMAND HELP" 'help)
    (option "--before" "BEFORE" 'ignore)
    (option "--after" "AFTER" 'ignore)
    (parse '("--before" "--help" "command" "--after")))))

(ert-deftest test-commander-option-required-argument-not-present-next-is-option ()
  (with-mock
   (mock (error "Option `%s` requires argument" "--help") :times 1)
   (stub foo)
   (commander
    (option "--foo" "FOO" 'ignore)
    (option "--help <foo>" "HELP" 'help)
    (parse '("--help" "--foo")))))

(ert-deftest test-commander-option-required-argument-not-present ()
  (with-mock
   (mock (error "Option `%s` requires argument" "--help") :times 1)
   (commander
    (option "--help <command>" "COMMAND HELP" 'help)
    (parse '("--help")))))

(ert-deftest test-commander-option-optional-argument-present ()
  (with-mock
   (mock (help "command") :times 1)
   (commander
    (option "--help [command]" "COMMAND HELP" 'help)
    (parse '("--help" "command")))))

(ert-deftest test-commander-option-optional-argument-not-present ()
  (with-mock
   (mock (help) :times 1)
   (commander
    (option "--help [command]" "COMMAND HELP" 'help)
    (parse '("--help")))))

(ert-deftest test-commander-option-optional-argument-not-present-with-default-value ()
  (with-mock
   (mock (help "all") :times 1)
   (commander
    (option "--help [command]" "COMMAND HELP" 'help "all")
    (parse '("--help")))))

(ert-deftest test-commander-option-optional-arguments-not-present-with-default-values ()
  (with-mock
   (mock (help "me" "clean") :times 1)
   (commander
    (option "--help [*]" "COMMAND HELP" 'help "me" "clean")
    (parse '("--help")))))

(ert-deftest test-commander-option-zero-or-more-no-arguments ()
  (with-mock
   (mock (help) :times 1)
   (commander
    (option "--help [*]" "HELP" 'help)
    (parse '("--help")))))

(ert-deftest test-commander-option-zero-or-more-with-arguments ()
  (with-mock
   (mock (help "foo" "bar" "baz") :times 1)
   (commander
    (option "--help [*]" "HELP" 'help)
    (parse '("--help" "foo" "bar" "baz")))))

(ert-deftest test-commander-option-one-or-more-no-arguments ()
  (with-mock
   (mock (error "Option `%s` requires at least one argument" "--help") :times 1)
   (commander
    (option "--help <*>" "HELP" 'help)
    (parse '("--help")))))

(ert-deftest test-commander-option-one-or-more-with-arguments ()
  (with-mock
   (mock (help "foo" "bar" "baz") :times 1)
   (commander
    (option "--help <*>" "HELP" 'help)
    (parse '("--help" "foo" "bar" "baz")))))

(ert-deftest test-commander-option-with-special-characters ()
  (with-mock
   (mock (option/f) :times 1)
   (mock (option/F) :times 1)
   (mock (option/0) :times 1)
   (mock (option/FOURTY-TWO) :times 1)
   (mock (option/fourty-two) :times 1)
   (mock (option/42) :times 1)
   (commander
    (option "-f" "..." 'option/f)
    (option "-F" "..." 'option/F)
    (option "-0" "..." 'option/0)
    (option "--FOURTY-TWO" "..." 'option/FOURTY-TWO)
    (option "--fourty-two" "..." 'option/fourty-two)
    (option "--42" "..." 'option/42)
    (parse '("-f" "-F" "-0" "--FOURTY-TWO" "--fourty-two" "--42")))))
