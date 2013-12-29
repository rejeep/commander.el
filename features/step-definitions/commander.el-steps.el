(Given "^this schedule:$"
  (lambda (schedule)
    (eval (read schedule))))

(Then "^usage should be:$"
  (lambda (usage)
    (should (string= usage (commander-usage)))))
