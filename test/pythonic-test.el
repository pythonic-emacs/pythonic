;;; pythonic-test.el --- pythonic test suite

;;; Commentary:

;;; Code:

(require 'ert)
(require 'pythonic)

;;; Executable.

(ert-deftest test-pythonic-executable ()
  "Check python executable detection."
  (should (s-equals-p "python" (pythonic-executable))))

(ert-deftest test-pythonic-executable-virtualenv ()
  "Check python executable detection."
  (let ((python-shell-virtualenv-path "/home/me/env"))
    (should (s-equals-p "/home/me/env/bin/python" (pythonic-executable)))))

(ert-deftest test-pythonic-executable-windows ()
  "Check python executable detection on windows platform."
  (let ((system-type 'windows-nt))
    (should (s-equals-p "pythonw" (pythonic-executable)))))

(ert-deftest test-pythonic-executable-windows-virtualenv ()
  "Check python executable detection on windows platform."
  (let ((system-type 'windows-nt)
        (python-shell-virtualenv-path "C:/env"))
    (should (s-equals-p "C:/env/Scripts/pythonw" (pythonic-executable)))))

(ert-deftest test-pythonic-executable-virtualenv-tramp ()
  "Check python executable detection on remote machine."
  (let ((python-shell-virtualenv-path "/localhost:/vagrant/env"))
    (should (s-equals-p "/vagrant/env/bin/python" (pythonic-executable)))))

;;; Command.

(ert-deftest test-pythonic-command ()
  "Check we get correct python command."
  (should (s-equals-p "python" (pythonic-command))))

(ert-deftest test-pythonic-command-remote ()
  "Check we run python on remote hosts over ssh."
  (let ((python-shell-virtualenv-path "/localhost:/vagrant/env"))
    (should (s-equals-p "ssh" (pythonic-command)))))

;;; Args.

(ert-deftest test-pythonic-args ()
  "Check pythonic arguments substitution."
  (should (equal '("-V") (pythonic-args "-V"))))

(ert-deftest test-pythonic-args-remote ()
  "Check pythonic arguments on remote machine."
  (let ((python-shell-virtualenv-path "/localhost:/path/to/env"))
    (should (equal '("/path/to/env/bin/python" "-V")
                   (pythonic-args "-V")))))

;;; Activate and deactivate virtual environment.

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
