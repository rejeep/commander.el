;; cask exec emacs --script examples/usage.el -- --help

(require 'commander (expand-file-name "commander" default-directory))

(defun cow (arg)
  (message "The cow says: %s" arg))

(defun dog (arg)
  (message "The dog says: %s" arg))

(commander
 (option "--cow <arg>" "Lorem ipsum dolor sit amet" cow)
 (option "--dog [arg]" "Consectetur adipiscing elit. Donec orci dolor" dog "Woff Woff")
 (option "--help, -h" "Luctus sed ullamcorper nec" commander-print-usage)
 (command "help" "Imperdiet eu eros" commander-print-usage)
 (command "foo <bar>" "Duis rhoncus luctus tellus" ignore)
 (command "barbaz [*]" "At porta odio congue id" ignore)
 (command "qux <*>" "Nunc tempus nisl ac erat vulputate vestibulum" ignore))
