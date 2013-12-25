;; cask exec emacs --script examples/auto-docs.el

(require 'commander (expand-file-name "commander" default-directory))

(defun foo ()
  "Print FOO.")

(defun bar ()
  "Print BAR.")

(defun baz ()
  "With BAZ.

And some more...")

(commander
 (default commander-print-usage-and-exit)
 (command "foo" foo)
 (command "bar" bar)
 (option "--baz" baz))
