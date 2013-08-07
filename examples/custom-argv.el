;; cask exec emacs --script examples/custom-argv.el

(require 'commander (expand-file-name "commander" default-directory))

(defun foo ()
  (message "FOO"))

(commander
 (option "--foo" "..." foo)
 (parse ("--foo")))
