(let ((current-directory (file-name-directory load-file-name)))
  (setq commander-test-path (expand-file-name "." current-directory))
  (setq commander-root-path (expand-file-name ".." current-directory)))

(require 'ert)
(require 'el-mock)
(require 'commander (expand-file-name "commander.el" commander-root-path))

(dolist (test-file (or argv (directory-files (expand-file-name commander-test-path) t "-test.el$")))
  (load test-file nil t))

(ert-run-tests-batch-and-exit t)
