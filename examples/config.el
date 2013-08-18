;; cask exec emacs --script examples/config.el -- --foo FOO-ARG

(require 'commander (expand-file-name "commander" default-directory))

(defun foo (arg)
  (message "FOO: %s" arg))

(defun bar (arg)
  (message "BAR: %s" arg))

(commander
 (config "examples/config.opts")
 (option "--foo <arg>" "..." foo)
 (option "--bar <arg>" "..." bar))
