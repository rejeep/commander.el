;;;; Options

(ert-deftest commander-config-test/single-option ()
  (with-sandbox
   (f-write-text "--foo" 'utf-8 "default.opts")
   (with-mock
    (mock (foo) :times 1)
    (mock (bar) :times 1)
    (commander
     (config "default.opts")
     (option "--foo" "..." foo)
     (option "--bar" "..." bar)
     (parse ("--bar"))))))

(ert-deftest commander-config-test/multiple-options ()
  (with-sandbox
   (f-write-text "--foo\n--bar" 'utf-8 "default.opts")
   (with-mock
    (mock (foo) :times 1)
    (mock (bar) :times 1)
    (mock (baz) :times 1)
    (commander
     (config "default.opts")
     (option "--foo" "..." foo)
     (option "--bar" "..." bar)
     (option "--baz" "..." baz)
     (parse ("--baz"))))))

(ert-deftest commander-config-test/option-with-argument ()
  (with-sandbox
   (f-write-text "--foo bar" 'utf-8 "default.opts")
   (with-mock
    (mock (foo "bar") :times 1)
    (mock (baz) :times 1)
    (commander
     (config "default.opts")
     (option "--foo <bar>" "..." foo)
     (option "--baz" "..." baz)
     (parse ("--baz"))))))

(ert-deftest commander-config-test/override-options-skank ()
  (with-sandbox
   (f-write-text "--foo bar" 'utf-8 "default.opts")
   (with-mock
    (mock (foo) :times 2)
    (commander
     (config "default.opts")
     (option "--foo <arg>" "..." foo)
     (parse ("--foo" "baz"))))))


;;;; Commands

(ert-deftest commander-config-test/command ()
  (with-sandbox
   (f-write-text "foo" 'utf-8 "default.opts")
   (with-mock
    (mock (foo) :times 1)
    (commander
     (config "default.opts")
     (command "foo" "..." foo)
     (parse nil)))))

(ert-deftest commander-config-test/command-with-argument ()
  (with-sandbox
   (f-write-text "foo bar" 'utf-8 "default.opts")
   (with-mock
    (mock (foo "bar") :times 1)
    (commander
     (config "default.opts")
     (command "foo [arg]" "..." foo)
     (parse nil)))))

(ert-deftest commander-config-test/override-command ()
  (with-sandbox
   (f-write-text "foo" 'utf-8 "default.opts")
   (with-mock
    (mock (bar) :times 1)
    (commander
     (config "default.opts")
     (command "foo" "..." foo)
     (command "bar" "..." bar)
     (parse ("bar"))))))

(ert-deftest commander-config-test/command-and-options ()
  (with-sandbox
   (f-write-text "foo bar\n--baz" 'utf-8 "default.opts")
   (with-mock
    (mock (foo "bar") :times 1)
    (mock (baz) :times 1)
    (mock (qux) :times 1)
    (commander
     (config "default.opts")
     (command "foo [arg]" "..." foo)
     (option "--baz" "..." baz)
     (option "--qux" "..." qux)
     (parse ("--qux"))))))


;;;; Misc

(ert-deftest commander-config-test/no-config-file ()
  (with-sandbox
   (with-mock
    (mock (foo "baz") :times 1)
    (commander
     (config "default.opts")
     (option "--foo <arg>" "..." foo)
     (parse ("--foo" "baz"))))))
