;;; pythonic-test.el --- pythonic test suite

;;; Commentary:

;;; Code:

(require 'ert)
(require 'pythonic)

(ert-deftest test-pythonic-executable ()
  "Check python executable detection."
  (should (s-equals-p "python" (pythonic--executable))))

(ert-deftest test-pythonic-executable-virtualenv ()
  "Check python executable detection."
  (let ((python-shell-virtualenv-path "/home/me/env"))
    (should (s-equals-p "/home/me/env/bin/python" (pythonic--executable)))))

(ert-deftest test-pythonic-executable-windows ()
  "Check python executable detection on windows platform."
  (let ((system-type 'windows-nt))
    (should (s-equals-p "pythonw" (pythonic--executable)))))

(ert-deftest test-pythonic-executable-windows-virtualenv ()
  "Check python executable detection on windows platform."
  (let ((system-type 'windows-nt)
        (python-shell-virtualenv-path "C:/env"))
    (should (s-equals-p "C:/env/Scripts/pythonw" (pythonic--executable)))))

(ert-deftest test-pythonic-executable-virtualenv-tramp ()
  "Check python executable detection on remote machine."
  (let ((python-shell-virtualenv-path "/localhost:/vagrant/env"))
    (should (s-equals-p "/vagrant/env/bin/python" (pythonic--executable)))))

(ert-deftest test-pythonic-activate ()
  "Check we can activate virtual environment."
  (let (python-shell-virtualenv-path)
    (pythonic-activate "/home/me/env")
    (should (s-equals-p python-shell-virtualenv-path "/home/me/env"))))

(ert-deftest test-pythonic-deactivate ()
  "Check we can deactivate virtual environment."
  (let ((python-shell-virtualenv-path "/home/me/env"))
    (pythonic-deactivate)
    (should (null python-shell-virtualenv-path))))

(provide 'pythonic-test)

;;; pythonic-test.el ends here
