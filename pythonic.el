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

(defun pythonic-remote-p ()
  "Determine remote or local virtual environment."
  (tramp-tramp-file-p default-directory))

(defun pythonic-remote-docker-p ()
  "Determine docker remote virtual environment."
  (and (pythonic-remote-p)
       (s-starts-with-p "/docker:" (pythonic-remote-connection))))

(defun pythonic-remote-vagrant-p ()
  "Determine vagrant remote virtual environment."
  (and (pythonic-remote-p)
       (s-equals-p (pythonic-remote-host) "localhost")
       (s-equals-p (pythonic-remote-user) "vagrant")))

(defun pythonic-remote-user ()
  "Get user of the connection to the remote python interpreter."
  (tramp-file-name-user (tramp-dissect-file-name (pythonic-remote-connection))))

(defun pythonic-remote-host ()
  "Get host of the connection to the remote python interpreter."
  (replace-regexp-in-string
   "#.*\\'" ""
   (tramp-file-name-host (tramp-dissect-file-name (pythonic-remote-connection)))))

(defun pythonic-remote-port ()
  "Get port of the connection to the remote python interpreter."
  (let ((hostname (tramp-file-name-host (tramp-dissect-file-name (pythonic-remote-connection)))))
    (when (s-contains-p "#" hostname)
      (string-to-number (replace-regexp-in-string "\\`.*#" "" hostname)))))

(defun pythonic-local-file-name (file)
  "Local FILE name with out tramp prefix."
  (if (tramp-tramp-file-p file)
      (tramp-file-name-localname (tramp-dissect-file-name file))
    file))

(defun pythonic-real-file-name (file)
  "Probably Remote FILE name with tramp prefix."
  (if (and (pythonic-remote-p)
           (not (tramp-tramp-file-p file)))
      (concat (pythonic-remote-connection) file)
    file))

(defun pythonic-real-directory-name (directory)
  "Generate `default-directory' FROM-DIRECTORY."
  (let ((default-directory (pythonic-real-file-name directory)))
    (f-full default-directory)))

(defun pythonic-remote-connection ()
  "Tramp connection string or nil."
  (when (pythonic-remote-p)
    (substring default-directory 0 (- (length default-directory)
                                      (length (pythonic-local-file-name default-directory))))))

(cl-defun pythonic-call-process (&key file buffer display args cwd)
  "Pythonic wrapper around `call-process'.

FILE is the input file. BUFFER is the output destination. DISPLAY
specifies to redisplay BUFFER on new output. ARGS is the list of
arguments passed to `call-process'. CWD will be working directory
for running process."
  (let ((default-directory (pythonic-real-directory-name (or cwd "~"))))
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
  (let ((default-directory (pythonic-real-directory-name (or cwd "~"))))
    (python-shell-with-environment
      (let ((process (apply 'start-file-process process buffer python-shell-interpreter args)))
        (when filter
          (set-process-filter process filter))
        (when sentinel
          (set-process-sentinel process sentinel))
        (set-process-query-on-exit-flag process query-on-exit)
        process))))

;;;###autoload
(defun pythonic-activate (virtualenv)
  "Activate python VIRTUALENV."
  (interactive "DEnv: ")
  (setq python-shell-virtualenv-root
	(pythonic-local-file-name (pythonic-real-directory-name virtualenv))))

;;;###autoload
(defun pythonic-deactivate ()
  "Deactivate python virtual environment."
  (interactive)
  (setq python-shell-virtualenv-root nil))

(provide 'pythonic)

;;; pythonic.el ends here
