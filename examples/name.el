;; carton exec emacs --script examples/name.el

(require 'commander (expand-file-name "commander.el" default-directory))

(commander
 (name "my-awesome-program")
 (default "help")
 (command "help" "Show usage information" 'commander-print-usage))
