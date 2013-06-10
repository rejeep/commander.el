;;; commander.el --- Command line parser

;; Copyright (C) 2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; Keywords: cli, argv
;; URL: http://github.com/rejeep/commander.el

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(eval-when-compile
  (require 'cl))
(require 's)
(require 'dash)

(defstruct commander-option flag description function required-argument optional-argument default-value)
(defstruct commander-command command description function)

(defvar commander-options nil)
(defvar commander-commands nil)
(defvar commander-parsing-done nil)

(defconst commander-option-re
  "\\(-[A-Za-z0-9-]\\|--[A-Za-z0-9][A-Za-z0-9-]+\\)"
  "Regex matching an option flag.")

(defun commander--option (flags description function &optional default-value)
  (let ((required-argument) (optional-argument))
    (-map
     (lambda (flag)
       (let ((matches (s-match (concat "^" commander-option-re " " "<\\([a-z]+\\)>" "$") flag)))
         (when matches
           (setq flag (nth 1 matches))
           (setq required-argument (nth 2 matches))))
       (let ((matches (s-match (concat "^" commander-option-re " " "\\[\\([a-z]+\\)\\]" "$") flag)))
         (when matches
           (setq flag (nth 1 matches))
           (setq optional-argument (nth 2 matches))))
       (add-to-list
        'commander-options
        (make-commander-option
         :flag flag
         :description description
         :function function
         :required-argument required-argument
         :optional-argument optional-argument
         :default-value default-value)))
     (-map 's-trim (s-split "," flags)))))

(defun commander--command (command description function)
  (add-to-list
   'commander-commands
   (make-commander-command
    :command command
    :description description
    :function function)))

(defun commander--find-option (option)
  (-first
   (lambda (commander-option)
     (equal (commander-option-flag commander-option) option))
   commander-options))

(defun commander--handle-options (arguments)
  (let ((i 0) (rest))
    (while (< i (length arguments))
      (let ((argument (nth i arguments)))
        (if (s-matches? (concat "^" commander-option-re "$") argument)
            (let ((commander-option (commander--find-option argument)))
              (cond (commander-option
                     (let ((function (commander-option-function commander-option)))
                       (cond ((commander-option-required-argument commander-option)
                              (setq i (1+ i))
                              (let ((next-argument (nth i arguments)))
                                (if next-argument
                                    (funcall function next-argument)
                                  (error "Option `%s` requires argument" argument))))
                             ((commander-option-optional-argument commander-option)
                              (let ((next-argument (nth (1+ i) arguments)))
                                (if (or (not next-argument) (s-matches? commander-option-re next-argument))
                                    (setq next-argument (commander-option-default-value commander-option))
                                  (setq i (1+ i)))
                                (funcall function next-argument)))
                             (t (funcall function)))))
                    (t (error "Option `%s` not available" argument))))
          (add-to-list 'rest argument)))
      (setq i (1+ i)))
    rest))

(defun commander--parse (arguments)
  (let ((rest (commander--handle-options arguments)))
    ;; TODO: Handle rest
    )
  (setq commander-parsing-done t))

(defmacro commander (&rest forms)
  "Specify option/command schema."
  (setq commander-options nil)
  (setq commander-commands nil)
  `(cl-flet ((option
           (flags description function &optional default-value)
           (commander--option flags description function default-value))
          (command
           (command description function)
           (commander--command command description function))
          (parse
           (arguments)
           (commander--parse arguments)))
     (setq commander-parsing-done nil)
     ,@forms
     (unless commander-parsing-done
       (commander--parse (cdr command-line-args-left)))))

(provide 'commander)

;;; commander.el ends here
