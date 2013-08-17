;;; ert-loader.el --- Load Ert if not included in Emacs

(require 'f)

(defvar commander-root-path
  (f-parent (f-parent load-file-name))
  "Path to root.")

(defvar commander-vendor-path
  (f-expand "vendor" commander-root-path)
  "Path to vendor.")

(unless (require 'ert nil 'noerror)
  (require 'ert (f-expand "ert" commander-vendor-path)))
