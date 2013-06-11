(require 'ert)
(require 'el-mock)
(require 'commander (expand-file-name "commander.el" (directory-file-name default-directory)))

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

(ert-deftest test-commander-required-argument-present ()
  (with-mock
   (mock (help "command") :times 1)
   (commander
    (option "--help <command>" "COMMAND HELP" 'help)
    (parse '("--help" "command")))))

(ert-deftest test-commander-required-argument-present-with-friends ()
  (with-mock
   (mock (help "command") :times 1)
   (commander
    (option "--help <command>" "COMMAND HELP" 'help)
    (option "--before" "BEFORE" 'ignore)
    (option "--after" "AFTER" 'ignore)
    (parse '("--before" "--help" "command" "--after")))))

(ert-deftest test-commander-required-argument-not-present-next-is-option ()
  (with-mock
   (mock (error "Option `%s` requires argument" "--help") :times 1)
   (stub foo)
   (commander
    (option "--foo" "FOO" 'ignore)
    (option "--help <foo>" "HELP" 'help)
    (parse '("--help" "--foo")))))

(ert-deftest test-commander-required-argument-not-present ()
  (with-mock
   (mock (error "Option `%s` requires argument" "--help") :times 1)
   (commander
    (option "--help <command>" "COMMAND HELP" 'help)
    (parse '("--help")))))

(ert-deftest test-commander-optional-argument-present ()
  (with-mock
   (mock (help "command") :times 1)
   (commander
    (option "--help [command]" "COMMAND HELP" 'help)
    (parse '("--help" "command")))))

(ert-deftest test-commander-optional-argument-not-present ()
  (with-mock
   (mock (help) :times 1)
   (commander
    (option "--help [command]" "COMMAND HELP" 'help)
    (parse '("--help")))))

(ert-deftest test-commander-optional-argument-not-present-with-default-value ()
  (with-mock
   (mock (help "all") :times 1)
   (commander
    (option "--help [command]" "COMMAND HELP" 'help "all")
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

(ert-deftest test-commander-mixed ()
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

(ert-deftest test-commander-command-simple ()
  (with-mock
   (mock (help) :times 1)
   (commander
    (command "help" "HELP" 'help)
    (parse '("help")))))

(ert-deftest test-commander-command-required-argument-present ()
  (with-mock
   (mock (help "show") :times 1)
   (commander
    (command "help <command>" "HELP" 'help)
    (parse '("help" "show")))))

(ert-deftest test-commander-command-required-argument-not-present ()
  (with-mock
   (mock (error "Command `%s` requires argument" "help") :times 1)
   (commander
    (command "help <command>" "HELP" 'help)
    (parse '("help")))))

(ert-deftest test-commander-command-one-or-more-no-arguments ()
  (with-mock
   (mock (error "Command `%s` requires at least one argument" "help") :times 1)
   (commander
    (command "help <*>" "HELP" 'help)
    (parse '("help")))))

(ert-deftest test-commander-command-one-or-more-with-arguments ()
  (with-mock
   (mock (help "foo" "bar" "baz") :times 1)
   (commander
    (command "help <*>" "HELP" 'help)
    (parse '("help" "foo" "bar" "baz")))))

(ert-deftest test-commander-command-optional-argument-present ()
  (with-mock
   (mock (help "show") :times 1)
   (commander
    (command "help [command]" "HELP" 'help)
    (parse '("help" "show")))))

(ert-deftest test-commander-command-optional-argument-not-present ()
  (with-mock
   (mock (help) :times 1)
   (commander
    (command "help [command]" "HELP" 'help)
    (parse '("help")))))

(ert-deftest test-commander-command-optional-argument-not-present-with-default-value ()
  (with-mock
   (mock (help "show") :times 1)
   (commander
    (command "help [command]" "HELP" 'help "show")
    (parse '("help")))))

(ert-deftest test-commander-command-zero-or-more-no-arguments ()
  (with-mock
   (mock (help) :times 1)
   (commander
    (command "help [*]" "HELP" 'help)
    (parse '("help")))))

(ert-deftest test-commander-command-zero-or-more-with-arguments ()
  (with-mock
   (mock (help "foo" "bar" "baz") :times 1)
   (commander
    (command "help [*]" "HELP" 'help)
    (parse '("help" "foo" "bar" "baz")))))

(ert-deftest test-commander-command-with-options ()
  (with-mock
   (mock (help "foo") :times 1)
   (mock (foo "bar") :times 1)
   (mock (qux) :times 1)
   (commander
    (option "--foo <arg>" "FOO" 'foo)
    (option "--qux" "QUX" 'qux)
    (command "help [*]" "HELP" 'help)
    (parse '("--foo" "bar" "help" "foo" "--qux")))))

(ert-deftest test-commander-command-not-registered ()
  (with-mock
   (mock (error "Command `%s` not available" "foo"))
   (commander
    (parse '("foo")))))

(ert-deftest test-commander-command-required-options-then-option-after ()
  (with-mock
   (mock (say "One" "Two" "Three") :times 1)
   (mock (four) :times 1)
   (commander
    (option "--say <*>" "..." 'say)
    (option "--four" "..." 'four)
    (parse '("--say" "One" "Two" "Three" "--four")))))

(ert-deftest test-commander-command-optional-options-then-option-after ()
  (with-mock
   (mock (say "One" "Two" "Three") :times 1)
   (mock (four) :times 1)
   (commander
    (option "--say [*]" "..." 'say)
    (option "--four" "..." 'four)
    (parse '("--say" "One" "Two" "Three" "--four")))))

(ert-run-tests-batch-and-exit)
