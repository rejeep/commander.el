(let ((current-directory (file-name-directory load-file-name)))
  (setq commander-root-path (expand-file-name ".." current-directory))
  (setq commander-test-path (expand-file-name "test" commander-root-path))
  (setq commander-vendor-path (expand-file-name "vendor" commander-root-path)))

(unless (require 'ert nil t)
  (require 'ert (expand-file-name "ert" commander-vendor-path)))
(require 'el-mock)
(require 'commander (expand-file-name "commander.el" commander-root-path))

(let ((test-files
       (or
        (mapcar
         (lambda (test-file)
           (expand-file-name test-file commander-root-path))
         (cdr argv))
        (directory-files (expand-file-name commander-test-path) t "-test.el$"))))
  (mapc
   (lambda (test-file)
     (load test-file nil t))
   test-files))

(ert-run-tests-batch-and-exit t)
