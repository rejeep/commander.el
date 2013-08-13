(require 'f)

(defvar commander-support-path
  (f-dirname load-file-name))

(defvar commander-features-path
  (f-parent commander-support-path))

(defvar commander-root-path
  (f-parent commander-features-path))

(defvar commander-vendor-path
  (f-expand "vendor" commander-root-path))

(add-to-list 'load-path commander-root-path)

(require 'espuds)
(unless (require 'ert nil 'noerror)
  (require 'ert (f-expand "ert" commander-vendor-path)))
(require 'commander)

(Fail
 (message "---------- FAIL ----------")
 (message (commander-usage))
 (message "--------------------------"))
