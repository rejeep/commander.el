(Given "^this schedule:$"
  (lambda (schedule)
    (eval (read schedule))))

(Then "^usage should be:$"
  (lambda (usage)
    (should (equal usage (commander-usage)))))
