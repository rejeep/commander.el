;;; commander.el --- Emacs command line parser

;; Copyright (C) 2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.1.2
;; Keywords: cli, argv
;; URL: http://github.com/rejeep/commander.el
;; Package-Requires: ((s "1.6.0") (dash "1.3.2"))

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

(require 'cl) ;; no eval-when-compile since commander--flet breaks
(require 's)
(require 'dash)

(defstruct commander-option
  flag
  description
  function
  default-values
  required
  optional
  zero-or-more
  one-or-more
  to-string)

(defstruct commander-command
  command
  description
  function
  default-values
  greedy
  required
  optional
  zero-or-more
  one-or-more
  to-string)

(defstruct commander-default-command
  command
  arguments)

(defvar commander-options nil)
(defvar commander-commands nil)
(defvar commander-name nil)
(defvar commander-default-command nil)

(defconst commander-option-re
  "\\(-[A-Za-z0-9-]\\|--[A-Za-z0-9][A-Za-z0-9-]+\\)"
  "Regex matching an option flag.")

(defconst commander-command-re
  "\\([A-Za-z0-9][A-Za-z0-9-]*\\)"
  "Regex matching an command.")

(defun commander--default (command arguments)
  (setq
   commander-default-command
   (make-commander-default-command
    :command command
    :arguments arguments)))

(defun commander--name (name)
  (setq commander-name name))

(defun commander--option (flags description function default-values)
  (let (required optional zero-or-more one-or-more)
    (-map
     (lambda (flag)
       (let ((to-string flag))
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
           :default-values default-values
           :required required
           :optional optional
           :zero-or-more zero-or-more
           :one-or-more one-or-more
           :to-string to-string))))
     (-map 's-trim (s-split "," flags)))))

(defun commander--command (command description function args)
  (let* (required
         optional
         zero-or-more
         one-or-more
         (to-string command)
         (default-values (-take-while 'stringp args)))
    (let ((matches (s-match (concat "^" commander-command-re " " "<\\(.+\\)>" "$") command)))
      (when matches
        (setq command (nth 1 matches))
        (when (nth 2 matches)
          (setq required t)
          (if (equal (nth 2 matches) "*")
              (setq one-or-more t)))))
    (let ((matches (s-match (concat "^" commander-command-re " " "\\[\\(.+\\)\\]" "$") command)))
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
      :default-values default-values
      :required required
      :optional optional
      :zero-or-more zero-or-more
      :one-or-more one-or-more
      :to-string to-string))))

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
                         (default-values (commander-option-default-values commander-option))
                         (required (commander-option-required commander-option))
                         (optional (commander-option-optional commander-option))
                         (zero-or-more (commander-option-zero-or-more commander-option))
                         (one-or-more (commander-option-one-or-more commander-option))
                         (option-arguments
                          (when (or required optional)
                            (if (or (and required one-or-more) (and optional zero-or-more))
                                (let ((next-arguments))
                                  (while (and (nth (1+ i) arguments) (not (s-matches? (s-concat "^" commander-option-re "$") (nth (1+ i) arguments))))
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
                               (apply function (or option-arguments default-values))
                             (if option-arguments
                                 (funcall function option-arguments)
                               (apply function default-values))))
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
              (default-values (commander-command-default-values commander-command))
              (required (commander-command-required commander-command))
              (optional (commander-command-optional commander-command))
              (zero-or-more (commander-command-zero-or-more commander-command))
              (one-or-more (commander-command-one-or-more commander-command)))
          (unless rest
            (setq rest default-values))
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

(defun commander--find-greedy (arguments)
  (-first
   (lambda (commander-command)
     (-any?
      (lambda (argument)
        (and
         (commander-command-greedy commander-command)
         (equal (commander-command-command commander-command) argument)))
      arguments))
   commander-commands))

(defun commander--parse (arguments)
  (let ((rest (commander--handle-options arguments)))
    (unless rest
      (if commander-default-command
          (let ((command (commander-default-command-command commander-default-command))
                (arguments (commander-default-command-arguments commander-default-command)))
            (setq rest (cons command arguments)))))
    (when rest (commander--handle-command rest))))

(defun commander--usage-command (commander-command)
  (let ((to-string (commander-command-to-string commander-command))
        (description (commander-command-description commander-command)))
    (s-concat " " (s-pad-right 20 " " to-string) description)))

(defun commander--usage-option (commander-option)
  (let ((to-string (commander-option-to-string commander-option))
        (description (commander-option-description commander-option)))
    (s-concat " " (s-pad-right 20 " " to-string) description)))

(defun commander-usage ()
  "Return usage information as a string."
  (let ((binary (or commander-name (file-name-nondirectory load-file-name)))
        (format-string "USAGE: %s COMMAND [OPTIONS]\n\nCOMMANDS:\n%s\n\nOPTIONS:\n%s\n")
        (commands
         (s-join "\n" (--map (commander--usage-command it) commander-commands)))
        (options
         (s-join "\n" (--map (commander--usage-option it) commander-options))))
    (format format-string binary commands options)))

(defun commander-print-usage ()
  "Print usage information."
  (message (commander-usage)))

(defmacro commander (&rest forms)
  `(let (commander-options
        commander-commands
        commander-default-command
        commander-parsing-done)
    (-each
     ',forms
     (lambda (form)
       (case (car form)
         (option
          (destructuring-bind (_ flags description function &rest default-values) form
            (commander--option flags description function default-values)))
         (command
          (destructuring-bind (_ command description function &rest args) form
            (commander--command command description function args)))
         (parse
          (destructuring-bind (_ arguments) form
            (commander--parse arguments)
            (setq commander-parsing-done t)))
         (name
          (destructuring-bind (_ name) form
            (commander--name name)))
         (default
           (destructuring-bind (_ command &rest arguments) form
             (commander--default command arguments))))))
    (unless commander-parsing-done
      (commander--parse (cdr command-line-args-left)))))

(provide 'commander)

;;; commander.el ends here
