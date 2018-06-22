;;; pythonic.el --- Utility functions for writing pythonic emacs package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2018 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/pythonic
;; Version: 0.1.1
;; Package-Requires: ((emacs "25") (s "1.9") (f "0.17.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the README for more details.

;;; Code:

(require 'python)
(require 'cl-lib)
(require 'tramp)
(require 's)
(require 'f)


;;; Connection predicates.

(defun pythonic-local-p ()
  "Determine local virtual environment."
  (not (pythonic-remote-p)))

(defun pythonic-remote-p ()
  "Determine remote virtual environment."
  (tramp-tramp-file-p (pythonic-aliased-path default-directory)))

(defun pythonic-remote-docker-p ()
  "Determine docker remote virtual environment."
  (and (pythonic-remote-p)
       (s-equals-p (pythonic-remote-method) "docker")))

(defun pythonic-remote-vagrant-p ()
  "Determine vagrant remote virtual environment."
  (and (pythonic-remote-p)
       (s-equals-p (pythonic-remote-host) "localhost")
       (s-equals-p (pythonic-remote-user) "vagrant")))


;;; Connection properties.

(defun pythonic-remote-method ()
  "Get tramp method of the connection to the remote python interpreter."
  (tramp-file-name-method (tramp-dissect-file-name (pythonic-aliased-path default-directory))))

(defun pythonic-remote-user ()
  "Get user of the connection to the remote python interpreter."
  (tramp-file-name-user (tramp-dissect-file-name (pythonic-aliased-path default-directory))))

(defun pythonic-remote-host ()
  "Get host of the connection to the remote python interpreter."
  (let ((hostname (tramp-file-name-host (tramp-dissect-file-name (pythonic-aliased-path default-directory)))))
    (replace-regexp-in-string "#.*\\'" "" hostname)))

(defun pythonic-remote-port ()
  "Get port of the connection to the remote python interpreter."
  (let ((hostname (tramp-file-name-host (tramp-dissect-file-name (pythonic-aliased-path default-directory)))))
    (when (s-contains-p "#" hostname)
      (string-to-number (replace-regexp-in-string "\\`.*#" "" hostname)))))


;;; File names.

(defvar pythonic-directory-aliases nil)

(defun pythonic-aliased-path (path)
  "Get aliased PATH."
  (let ((alias-tuple (cl-find-if
                      (lambda (it)
                        (or (f-same-p (car it) path)
                            (f-ancestor-of-p (car it) path)))
                      pythonic-directory-aliases)))
    (if (null alias-tuple)
        path
      (f-join (cadr alias-tuple)
              (substring path (length (car alias-tuple)))))))

(defun pythonic-unaliased-path (alias)
  "Get real path from ALIAS."
  (let ((alias-tuple (cl-find-if
                      (lambda (it)
                        (or (f-same-p (cadr it) alias)
                            (f-ancestor-of-p (cadr it) alias)))
                      pythonic-directory-aliases)))
    (if (null alias-tuple)
        alias
      (f-join (car alias-tuple)
              (substring alias (min (length (cadr alias-tuple))
                                    (length alias)))))))

(defun pythonic-has-alias-p (path)
  "Check if given PATH has alias."
  (not (null (cl-find-if
              (lambda (it)
                (or (f-same-p (car it) path)
                    (f-ancestor-of-p (car it) path)))
              pythonic-directory-aliases))))

(defun pythonic-python-readable-file-name (filename)
  "Emacs to Python FILENAME conversion.
Take FILENAME from the perspective of the localhost and translate
it to the FILENAME Python process can read.  Python can be
running locally or remotely.  FILENAME can have local or tramp
format.  Result will have local format."
  (let ((alias (pythonic-aliased-path filename)))
    (if (tramp-tramp-file-p alias)
        (tramp-file-name-localname (tramp-dissect-file-name alias))
      alias)))

(defun pythonic-emacs-readable-file-name (filename)
  "Python to Emacs FILENAME conversion.
Take FILENAME from the perspective of the python interpreter and
translate it to the FILENAME Emacs `find-file' command can
understand.  Python can be running locally or remotely.  FILENAME
should have local format.  Result can have local or tramp
format."
  (when (tramp-tramp-file-p filename)
    (error "%s can not be tramp path" filename))
  (if (pythonic-remote-p)
      (let* ((directory (pythonic-aliased-path default-directory))
             (connection (substring directory 0
                                    (- (length directory)
                                       (length (tramp-file-name-localname (tramp-dissect-file-name directory)))))))
        (pythonic-unaliased-path (concat connection filename)))
    filename))


;;; Processes.

(cl-defun pythonic-call-process (&key file buffer display args cwd)
  "Pythonic wrapper around `call-process'.

FILE is the input file. BUFFER is the output destination. DISPLAY
specifies to redisplay BUFFER on new output. ARGS is the list of
arguments passed to `call-process'. CWD will be working directory
for running process."
  (let ((default-directory (pythonic-aliased-path (or cwd default-directory))))
    (python-shell-with-environment
      (apply 'process-file python-shell-interpreter file buffer display args))))

(cl-defun pythonic-start-process (&key process buffer args cwd filter sentinel (query-on-exit t))
  "Pythonic wrapper around `start-process'.

PROCESS is a name of the created process. BUFFER is a output
destination. ARGS are the list of args passed to
`start-process'. CWD will be working directory for running
process.  FILTER must be a symbol of process filter function if
necessary.  SENTINEL must be a symbol of process sentinel
function if necessary.  QUERY-ON-EXIT will be corresponding
process flag."
  (let ((default-directory (pythonic-aliased-path (or cwd default-directory))))
    (python-shell-with-environment
      (let ((process (apply 'start-file-process process buffer python-shell-interpreter args)))
        (when filter
          (set-process-filter process filter))
        (when sentinel
          (set-process-sentinel process sentinel))
        (set-process-query-on-exit-flag process query-on-exit)
        process))))


;;; Commands.

;;;###autoload
(defun pythonic-activate (virtualenv)
  "Activate python VIRTUALENV."
  (interactive "DEnv: ")
  (setq python-shell-virtualenv-root (pythonic-python-readable-file-name virtualenv)))

;;;###autoload
(defun pythonic-deactivate ()
  "Deactivate python virtual environment."
  (interactive)
  (setq python-shell-virtualenv-root nil))

(provide 'pythonic)

;;; pythonic.el ends here
