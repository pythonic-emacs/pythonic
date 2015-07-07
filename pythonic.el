;;; pythonic.el --- Utility functions for writing pythonic emacs package.

;; Copyright (C) 2015 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/pythonic
;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (dash "2.11") (s "1.9") (f "0.17.2"))

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
(require 'tramp)
(require 'tramp-sh)
(require 'cl-lib)
(require 'dash)
(require 's)
(require 'f)

(defvaralias 'pythonic-environment
  (if (boundp 'python-shell-virtualenv-root)
      'python-shell-virtualenv-root
    'python-shell-virtualenv-path)
  "Alias to `python.el' virtualenv variable.")

(defun pythonic-remote-p ()
  "Determine remote or local virtual environment."
  (if pythonic-environment
      (tramp-tramp-file-p pythonic-environment)
    (tramp-tramp-file-p python-shell-interpreter)))

(defun pythonic-file-name (file)
  "Normalized FILE location with out tramp prefix."
  (if (tramp-tramp-file-p file)
      (tramp-file-name-localname
       (tramp-dissect-file-name file))
    file))

(defun pythonic-tramp-connection ()
  "Tramp connection string or nil."
  (-when-let* ((vars (--filter (tramp-tramp-file-p it)
                               (list pythonic-environment
                                     python-shell-interpreter)))
               (var (car vars)))
    (substring var 0 (- (length var) (length (pythonic-file-name var))))))

(defun pythonic-executable ()
  "Python executable."
  (let* ((windowsp (eq system-type 'windows-nt))
         (python (if windowsp "pythonw" "python"))
         (bin (if windowsp "Scripts" "bin")))
    (if pythonic-environment
        (f-join (pythonic-file-name pythonic-environment) bin python)
      (pythonic-file-name python-shell-interpreter))))

(defun pythonic-default-directory (&optional from-directory)
  "Generate `default-directory' FROM-DIRECTORY."
  (concat (pythonic-tramp-connection) (or from-directory "~")))

(defun pythonic-set-process-environment ()
  "Set process environment variables from `python-mode' settings.
It will use `python-shell-exec-path' for PATH variable,
`python-shell-extra-pythonpaths' for PYTHONPATH variable."
  (if (pythonic-remote-p)
      (progn
        (pythonic-set-pythonpath-variable-tramp)
        (pythonic-set-path-variable-tramp)
        (pythonic-set-extra-variables-tramp))
    (pythonic-set-pythonpath-variable)
    (pythonic-set-path-variable)
    (pythonic-set-extra-variables)))

(defun pythonic-set-pythonpath-variable ()
  "Set PYTHONPATH variable from `python-shell-extra-pythonpaths' variable."
  (-let* ((pythonpath (or (getenv "PYTHONPATH") ""))
          (pythonpaths (s-split path-separator pythonpath t))
          (external-pythonpaths (--remove (member it python-shell-extra-pythonpaths)
                                          pythonpaths))
          (new-pythonpaths (append python-shell-extra-pythonpaths
                                   external-pythonpaths))
          (new-pythonpath (s-join path-separator new-pythonpaths)))
    (setenv "PYTHONPATH" new-pythonpath)))

(defun pythonic-set-pythonpath-variable-tramp ()
  "Set PYTHONPATH variable from `python-shell-extra-pythonpaths' variable on remote host."
  (let ((connection (tramp-dissect-file-name (pythonic-tramp-connection))))
    (tramp-send-command
     connection
     (format
      "export PYTHONPATH=%s"
      (s-join ":"
              (--> (progn
                     (tramp-send-command connection "echo $PYTHONPATH")
                     (with-current-buffer (tramp-get-connection-buffer connection)
                       (buffer-string)))
                   (s-trim it)
                   (s-split ":" it t)
                   (--remove (member it python-shell-extra-pythonpaths) it)
                   (append python-shell-extra-pythonpaths it)))))))

(defun pythonic-set-path-variable ()
  "Set PATH according to `python-shell-exec-path'."
  (setenv "PATH"
          (s-join path-separator
                  (-remove 'null
                           (append python-shell-exec-path
                                   (list (getenv "PATH")))))))

(defun pythonic-set-path-variable-tramp ()
  "Set PATH according to `python-shell-exec-path' on remote host."
  (let* ((vec (tramp-dissect-file-name (pythonic-tramp-connection)))
         (path (append python-shell-exec-path (tramp-get-remote-path vec))))
    (tramp-set-connection-property vec "remote-path" path)
    (tramp-set-remote-path vec)))

(defun pythonic-set-extra-variables ()
  "Set environment variables according to `python-shell-process-environment'."
  (dolist (env python-shell-process-environment)
    (let ((env (s-split "=" env)))
      (setenv (car env) (cadr env)))))

(defun pythonic-set-extra-variables-tramp ()
  "Set remote environment variables from `python-shell-process-environment'."
  (dolist (env python-shell-process-environment)
    (tramp-send-command
     (tramp-dissect-file-name (pythonic-tramp-connection))
     (format "export %s" env))))

(cl-defun call-pythonic (&key file buffer display args cwd)
  "Pythonic wrapper around `call-process'.

FILE is the input file. BUFFER is the output destination. DISPLAY
specifies to redisplay BUFFER on new output. ARGS is the list of
arguments passed to `call-process'. CWD will be working directory
for running process."
  (let ((default-directory (pythonic-default-directory cwd))
        (process-environment (copy-sequence process-environment)))
    (pythonic-set-process-environment)
    (apply 'process-file (pythonic-executable) file buffer display args)))

(cl-defun start-pythonic (&key process buffer args cwd filter sentinel)
  "Pythonic wrapper around `start-process'.

PROCESS is a name of the created process. BUFFER is a output
destination. ARGS are the list of args passed to
`start-process'. CWD will be working directory for running
process.  FILTER must be a symbol of process filter function if
necessary.  SENTINEL must be a symbol of process sentinel
function if necessary."
  (let ((default-directory (pythonic-default-directory cwd))
        (process-environment (copy-sequence process-environment)))
    (pythonic-set-process-environment)
    (let ((process (apply 'start-file-process process buffer (pythonic-executable) args)))
      (when filter
        (set-process-filter process filter))
      (when sentinel
        (set-process-sentinel process sentinel))
      process)))

;;;###autoload
(defun pythonic-activate (virtualenv)
  "Activate python VIRTUALENV."
  (interactive "DEnv: ")
  (setq pythonic-environment virtualenv))

;;;###autoload
(defun pythonic-deactivate ()
  "Deactivate python virtual environment."
  (interactive)
  (setq pythonic-environment nil))

(provide 'pythonic)

;;; pythonic.el ends here
