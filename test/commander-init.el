(require 'el-mock)
(eval-when-compile
  (require 'cl)) ;; for el-mock

(defvar commander-test/test-path
  (f-dirname load-file-name))

(defvar commander-test/root-path
  (f-parent commander-test/test-path))

(load (f-expand "commander" commander-test/root-path))
