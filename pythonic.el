;;; pythonic.el --- Utility functions for writing pythonic emacs package.

;; Copyright (C) 2015 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/pythonic
;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (f "0.17.2"))

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
(require 'dash)
(require 'f)

(defun pythonic-executable ()
  "Python executable."
  (let* ((windowsp (eq system-type 'windows-nt))
         (python (if windowsp "pythonw" "python"))
         (bin (if windowsp "Scripts" "bin")))
    (--if-let python-shell-virtualenv-path
        (f-join (if (tramp-tramp-file-p it)
                    (tramp-file-name-localname (tramp-dissect-file-name it))
                  it)
                bin
                python)
      python)))

;; (let ((default-directory "/drweb:"))
;;   (get-buffer-create "a")
;;   (start-file-process "a" "a" "/opt/multiscanner/virtualenv/multiscanner/bin/python" "calc.py")
;;   (pop-to-buffer "a"))

(provide 'pythonic)

;;; pythonic.el ends here
