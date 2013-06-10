;; carton exec emacs --script examples/special-argv.el

(require 'commander (expand-file-name "commander.el" default-directory))

(defun foo ()
  (message "FOO"))

(commander
 (option "--foo" "..." 'foo)
 (parse '("--foo")))
