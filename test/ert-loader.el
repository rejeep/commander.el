;;; ert-loader.el - Load Ert if not included in Emacs

(require 'f)

(defvar f-root-path
  (f-parent (f-parent load-file-name))
  "Path to root.")

(defvar f-vendor-path
  (f-expand "vendor" f-root-path)
  "Path to vendor.")

(unless (require 'ert nil 'noerror)
  (require 'ert (f-expand "ert" f-vendor-path)))
