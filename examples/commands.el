;; carton exec emacs --script examples/commands.el -- sum 1 2 3 4 5 --say Muuuu

(require 'commander (expand-file-name "commander.el" default-directory))

(defun say (arg)
  (message "Saying: %s" arg))

(defun sum (&rest args)
  (message "Sum is: %d" (apply '+ (mapcar 'string-to-int args))))

(commander
 (option "--say <*>" "..." 'say)
 (command "sum [args]" "..." 'sum 0))
