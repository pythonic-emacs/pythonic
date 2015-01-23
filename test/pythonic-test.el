;;; pythonic-test.el --- pythonic test suite

;;; Commentary:

;;; Code:

(require 'ert)
(require 'pythonic)

(ert-deftest test-pythonic-executable ()
  "Check python executable detection."
  (should (s-equals? "python" (pythonic-executable))))

(ert-deftest test-pythonic-executable-virtualenv ()
  "Check python executable detection."
  (let ((python-shell-virtualenv-path "/home/me/env"))
    (should (s-equals? "/home/me/env/bin/python" (pythonic-executable)))))

(ert-deftest test-pythonic-executable-windows ()
  "Check python executable detection on windows platform."
  (let ((system-type 'windows-nt))
    (should (s-equals? "pythonw" (pythonic-executable)))))

(ert-deftest test-pythonic-executable-windows-virtualenv ()
  "Check python executable detection on windows platform."
  (let ((system-type 'windows-nt)
        (python-shell-virtualenv-path "C:/env"))
    (should (s-equals? "C:/env/Scripts/pythonw" (pythonic-executable)))))

(provide 'pythonic-test)

;;; pythonic-test.el ends here
