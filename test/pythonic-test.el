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

(ert-deftest test-pythonic-executable-windows-virtualenv ()
  "Check python executable detection on windows platform."
  (let ((system-type 'windows-nt)
        (python-shell-virtualenv-path "C:/env"))
    (should (s-equals-p "C:/env/Scripts/pythonw" (pythonic-executable)))))

(ert-deftest test-pythonic-executable-virtualenv-tramp ()
  "Check python executable detection on remote machine."
  (let ((python-shell-virtualenv-path "/localhost:/vagrant/env"))
    (should (s-equals-p "/vagrant/env/bin/python" (pythonic-executable)))))

(ert-deftest test-pythonic-executable-interpreter ()
  "Check python executable detection with interpreter specified."
  (let ((python-shell-interpreter "/path/to/the/python"))
    (should (s-equals-p "/path/to/the/python" (pythonic-executable)))))

(ert-deftest test-pythonic-executable-interpreter-remote ()
  "Check python executable detection on remote host."
  (let ((python-shell-interpreter "/localhost:/path/to/the/python"))
    (should (s-equals-p "/path/to/the/python" (pythonic-executable)))))

;;; Tramp.

(ert-deftest test-pythonic-remote-p ()
  "Check we can detect remote settings properly."
  (should-not (pythonic-remote-p)))

(ert-deftest test-pythonic-remote-p-virtualenv ()
  "Check remote virtual environment."
  (let ((python-shell-virtualenv-path "/localhost:/path/to/env"))
    (should (pythonic-remote-p))))

(ert-deftest test-pythonic-remote-p-interpreter ()
  "Check remote interpreter."
  (let ((python-shell-interpreter "/localhost:/path/to/python"))
    (should (pythonic-remote-p))))

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
    (should-not python-shell-virtualenv-path)))

(provide 'pythonic-test)

;;; pythonic-test.el ends here
