;;; pythonic-test.el --- pythonic test suite

;;; Commentary:

;;; Code:

(require 'ert)
(require 'pythonic)

;;; Defaults.

(ert-deftest test-pythonic-executable ()
  "Basic python executable."
  (should (s-equals-p "python" (pythonic-executable))))

;;; Interpreter.

(ert-deftest test-pythonic-executable-interpreter ()
  "Interpreter python executable on the local host."
  (let ((python-shell-interpreter "/path/to/the/python"))
    (should (s-equals-p "/path/to/the/python" (pythonic-executable)))))

;;; Process environment.

;;; PYTHONPATH.

;;; Exec path.

;;; Virtual environment.

(ert-deftest test-pythonic-executable-virtualenv ()
  "Virtual environment python executable."
  (let ((python-shell-virtualenv-path "/home/me/env"))
    (should (s-equals-p "/home/me/env/bin/python" (pythonic-executable)))))

(ert-deftest test-pythonic-executable-virtualenv-windows ()
  "Virtual environment python executable on the windows platform."
  (let ((system-type 'windows-nt)
        (python-shell-virtualenv-path "C:/env"))
    (should (s-equals-p "C:/env/Scripts/pythonw" (pythonic-executable)))))

(ert-deftest test-pythonic-executable-virtualenv-remote ()
  "Virtual environment python executable on the remote host."
  (let ((python-shell-virtualenv-path "/localhost:/vagrant/env"))
    (should (s-equals-p "/vagrant/env/bin/python" (pythonic-executable)))))

;;; Processes.

(ert-deftest test-call-pythonic ()
  "Run synchronous python process."
  (should (eq 0 (call-pythonic :buffer "*out*"
                               :args '("-V")))))

(ert-deftest test-call-pythonic-cwd ()
  "Pass current working directory to the process."
  (call-pythonic :buffer "*out1*"
                 :cwd "~"
                 :args '("-c" "from __future__ import print_function; import os; print(os.getcwd())"))
  (should (s-equals-p (s-concat (expand-file-name "~") "\n")
                      (with-current-buffer "*out1*"
                        (buffer-string)))))

(ert-deftest test-start-pythonic ()
  "Run asynchronous python process."
  (should (equal '("python" "-V")
                 (process-command
                  (start-pythonic :process "out2"
                                  :buffer "*out2*"
                                  :args '("-V"))))))

(ert-deftest test-start-pythonic-cwd ()
  "Run asynchronous python process with working directory specified."
  (let ((process (start-pythonic :process "out3"
                                 :buffer "*out3*"
                                 :cwd "~"
                                 :args '("-c" "from __future__ import print_function; import os; print(os.getcwd())"))))
    (while (process-live-p process)
      (accept-process-output process))
    (should (s-equals-p (s-concat (expand-file-name "~") "\n\nProcess out3 finished\n")
                        (with-current-buffer "*out3*"
                          (buffer-string))))))


;;; Activate and deactivate virtual environment.

(ert-deftest test-pythonic-activate ()
  "Activate virtual environment."
  (let (python-shell-virtualenv-path)
    (pythonic-activate "/home/me/env")
    (should (s-equals-p python-shell-virtualenv-path "/home/me/env"))))

(ert-deftest test-pythonic-deactivate ()
  "Deactivate virtual environment."
  (let ((python-shell-virtualenv-path "/home/me/env"))
    (pythonic-deactivate)
    (should-not python-shell-virtualenv-path)))

(provide 'pythonic-test)

;;; pythonic-test.el ends here
