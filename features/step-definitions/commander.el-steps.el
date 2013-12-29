(Given "^this schedule:$"
  (lambda (schedule)
    (eval (read schedule))))

(Then "^usage should be:$"
  (lambda (usage)
    (should (string= usage (commander-usage)))))

(Then "^usage for \"\\([^\"]+\\)\" should be:$"
  (lambda (command-name usage)
    (should (string= usage (s-join "\n" (commander-usage-for command-name))))))

(Then "^usage for \"\\([^\"]+\\)\" should error$"
  (lambda (command-name)
    (should-error (commander-usage-for command-name))))
