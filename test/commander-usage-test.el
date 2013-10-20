(ert-deftest commander-usage-test/print-and-exit ()
  (with-mock
   (mock (commander-print-usage))
   (mock (kill-emacs 0))
   (commander-print-usage-and-exit)))

(ert-deftest commander-usage-test/print-and-exit-with-arg ()
  (with-mock
   (mock (commander-print-usage))
   (mock (kill-emacs 10))
   (commander-print-usage-and-exit 10)))
