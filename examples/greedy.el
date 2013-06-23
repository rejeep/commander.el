;; carton exec emacs --script examples/greedy.el -- eat food desert
;; carton exec emacs --script examples/greedy.el -- --opt foo bar eat food desert --cheese

(require 'commander (expand-file-name "commander.el" default-directory))

(defun eat (&rest args)
  (message "Eating %s" (mapconcat 'identity args ", and then ")))

(defun opt (&rest args)
  (message "Options: %s" (mapconcat 'identity args ", ")))

(commander
 (option "--opt [*]" "..." 'opt)
 (command "eat [args]" "..." 'eat :greedy t))
