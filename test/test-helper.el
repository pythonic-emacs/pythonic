;;; test-helper.el --- ert-runner test helper

;;; Commentary:

;;; Code:

(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

(undercover "*.el")

(defun empty (&rest ignored)
  "Do nothing.  All arguments will be IGNORED.")

(provide 'test-helper)

;;; test-helper.el ends here
