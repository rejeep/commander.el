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

(unload-feature 'commander 'force)

(require 'espuds)
(unless (require 'ert nil 'noerror)
  (require 'ert (f-expand "ert" commander-vendor-path)))
(require 'commander)

(defun foo ()
  "Return FOO.")

(defun bar ()
  "Print BAR.

More info.
And more info.")

(defun baz ())

(defun qux ())

(Before
 (setq commander-args nil))
