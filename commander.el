;;; commander.el --- Emacs command line parser

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

(defstruct commander-option
  flag
  description
  function
  default-value
  required
  optional
  zero-or-more
  one-or-more)

(defstruct commander-command
  command
  description
  function
  default-value
  required
  optional
  zero-or-more
  one-or-more)

(defvar commander-options nil)
(defvar commander-commands nil)
(defvar commander-parsing-done nil)

(defconst commander-option-re
  "\\(-[A-Za-z0-9-]\\|--[A-Za-z0-9][A-Za-z0-9-]+\\)"
  "Regex matching an option flag.")

(defun commander--option (flags description function &optional default-value)
  (let (required optional zero-or-more one-or-more)
    (-map
     (lambda (flag)
       (let ((matches (s-match (concat "^" commander-option-re " " "<\\(.+\\)>" "$") flag)))
         (when matches
           (setq flag (nth 1 matches))
           (when (nth 2 matches)
             (setq required t)
             (if (equal (nth 2 matches) "*")
                 (setq one-or-more t)))))
       (let ((matches (s-match (concat "^" commander-option-re " " "\\[\\(.+\\)\\]" "$") flag)))
         (when matches
           (setq flag (nth 1 matches))
           (when (nth 2 matches)
             (setq optional t)
             (if (equal (nth 2 matches) "*")
                 (setq zero-or-more t)))))
       (add-to-list
        'commander-options
        (make-commander-option
         :flag flag
         :description description
         :function function
         :default-value default-value
         :required required
         :optional optional
         :zero-or-more zero-or-more
         :one-or-more one-or-more)))
     (-map 's-trim (s-split "," flags)))))

(defun commander--command (command description function &optional default-value)
  (let (required optional zero-or-more one-or-more)
    (let ((matches (s-match "^\\([a-z]+\\) <\\(.+\\)>$" command)))
      (when matches
        (setq command (nth 1 matches))
        (when (nth 2 matches)
          (setq required t)
          (if (equal (nth 2 matches) "*")
              (setq one-or-more t)))))
    (let ((matches (s-match "^\\([a-z]+\\) \\[\\(.+\\)\\]$" command)))
      (when matches
        (setq command (nth 1 matches))
        (when (nth 2 matches)
          (setq optional t)
          (if (equal (nth 2 matches) "*")
              (setq zero-or-more t)))))
    (add-to-list
     'commander-commands
     (make-commander-command
      :command command
      :description description
      :function function
      :default-value default-value
      :required required
      :optional optional
      :zero-or-more zero-or-more
      :one-or-more one-or-more))))

(defun commander--find-option (option)
  (-first
   (lambda (commander-option)
     (equal (commander-option-flag commander-option) option))
   commander-options))

(defun commander--find-command (command)
  (-first
   (lambda (commander-command)
     (equal (commander-command-command commander-command) command))
   commander-commands))

(defun commander--handle-options (arguments)
  (let ((i 0) (rest))
    (while (< i (length arguments))
      (let ((argument (nth i arguments)))
        (if (s-matches? (concat "^" commander-option-re "$") argument)
            (let ((commander-option (commander--find-option argument)))
              (if commander-option
                  (let* ((function (commander-option-function commander-option))
                         (default-value (commander-option-default-value commander-option))
                         (required (commander-option-required commander-option))
                         (optional (commander-option-optional commander-option))
                         (zero-or-more (commander-option-zero-or-more commander-option))
                         (one-or-more (commander-option-one-or-more commander-option))
                         (option-arguments
                          (when (or required optional)
                            (if (or (and required one-or-more) (and optional zero-or-more))
                                (let ((next-arguments))
                                  (while (and (nth (1+ i) arguments) (not (s-matches? commander-option-re (nth (1+ i) arguments))))
                                    (setq i (1+ i))
                                    (add-to-list 'next-arguments (nth i arguments) t))
                                  next-arguments)
                              (when (and (nth (1+ i) arguments) (not (s-matches? commander-option-re (nth (1+ i) arguments))))
                                (setq i (1+ i))
                                (nth i arguments))))))
                    (cond (required
                           (if option-arguments
                               (if one-or-more
                                   (apply function option-arguments)
                                 (funcall function option-arguments))
                             (if one-or-more
                                 (error "Option `%s` requires at least one argument" argument)
                               (error "Option `%s` requires argument" argument))))
                          (optional
                           (if zero-or-more
                               (apply function option-arguments)
                             (funcall function (or option-arguments default-value))))
                          (t (funcall function))))
                (error "Option `%s` not available" argument)))
          (add-to-list 'rest argument t)))
      (setq i (1+ i)))
    rest))

(defun commander--handle-command (arguments)
  (let* ((command (car arguments))
         (rest (cdr arguments))
         (commander-command (commander--find-command command)))
    (if commander-command
        (let ((function (commander-command-function commander-command))
              (default-value (commander-command-default-value commander-command))
              (required (commander-command-required commander-command))
              (optional (commander-command-optional commander-command))
              (zero-or-more (commander-command-zero-or-more commander-command))
              (one-or-more (commander-command-one-or-more commander-command)))
          (cond (required
                 (if rest
                     (apply function rest)
                   (if one-or-more
                       (error "Command `%s` requires at least one argument" command)
                     (error "Command `%s` requires argument" command))))
                (optional
                 (apply function rest))
                (t
                 (funcall function))))
      (error "Command `%s` not available" command))))

(defun commander--parse (arguments)
  (let ((rest (commander--handle-options arguments)))
    (when rest (commander--handle-command rest)))
  (setq commander-parsing-done t))

(defmacro commander (&rest forms)
  "Specify option/command schema."
  `(cl-flet ((option
              (flags description function &optional default-value)
              (commander--option flags description function default-value))
             (command
              (command description function &optional default-value)
              (commander--command command description function default-value))
             (parse
              (arguments)
              (commander--parse arguments)))
     (setq commander-options nil)
     (setq commander-commands nil)
     (setq commander-parsing-done nil)
     ,@forms
     (unless commander-parsing-done
       (commander--parse (cdr command-line-args-left)))))

(provide 'commander)

;;; commander.el ends here
