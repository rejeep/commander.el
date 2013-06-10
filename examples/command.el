;; carton exec emacs --script examples/commands.el -- sum 1 2 3 4 5 --say One Two Three --four

(require 'commander (expand-file-name "commander.el" default-directory))

(defun say (&rest args)
  (message "Saying: %s" (mapconcat 'identity args " ")))

(defun four ()
  (say "Four"))

(defun sum (&rest args)
  (message "Sum is: %d" (apply '+ (mapcar 'string-to-int args))))

(commander
 (option "--say <*>" "..." 'say)
 (option "--four" "..." 'four)
 (command "sum [args]" "..." 'sum 0))
