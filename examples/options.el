;; carton exec emacs --script examples/options.el -- --cow Muuuu --dog

(require 'commander (expand-file-name "commander.el" default-directory))

(defun cow (arg)
  (message "The cow says: %s" arg))

(defun dog (arg)
  (message "The dog says: %s" arg))

(commander
 (option "--cow <arg>" "..." 'cow)
 (option "--dog [arg]" "..." 'dog "Woff Woff"))
