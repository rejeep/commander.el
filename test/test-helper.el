(require 'f)
(require 'dash)

(defvar commander-sandbox-path
  (f-expand "sandbox" (f-dirname load-file-name)))

(defmacro with-sandbox (&rest body)
  `(let ((default-directory commander-sandbox-path))
     (when (f-dir? commander-sandbox-path)
       (f-delete commander-sandbox-path :force))
     (f-mkdir commander-sandbox-path)
     ,@body))
