;; carton exec emacs --script examples/default.el -- show
;; carton exec emacs --script examples/default.el -- hide
;; carton exec emacs --script examples/default.el

(require 'commander (expand-file-name "commander.el" default-directory))

(defun show (arg)
  (message "showing: %s" arg))

(defun hide (arg)
  (message "hiding: %s" arg))

(commander
 (default "show" "stuffing")
 (command "show <stuff>" "Show stuff" 'show)
 (command "hide <stuff>" "Hide stuff" 'hide))
