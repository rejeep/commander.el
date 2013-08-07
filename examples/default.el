;; cask exec emacs --script examples/default.el -- show
;; cask exec emacs --script examples/default.el -- hide
;; cask exec emacs --script examples/default.el

(require 'commander (expand-file-name "commander" default-directory))

(defun show (arg)
  (message "showing: %s" arg))

(defun hide (arg)
  (message "hiding: %s" arg))

(commander
 (default "show" "stuffing")
 (command "show <stuff>" "Show stuff" show)
 (command "hide <stuff>" "Hide stuff" hide))
