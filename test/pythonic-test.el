;;; pythonic-test.el --- pythonic test suite

;;; Commentary:

;;; Code:

(require 'ert)
(require 'pythonic)
(require 's)

;;; Executable.

(ert-deftest test-pythonic-executable ()
  "Basic python executable."
  (should (s-equals-p "python" (pythonic-executable))))

(ert-deftest test-pythonic-executable-interpreter ()
  "Python executable on the local host."
  (let ((python-shell-interpreter "/path/to/the/python"))
    (should (s-equals-p "/path/to/the/python" (pythonic-executable)))))

(ert-deftest test-pythonic-executable-interpreter-remote ()
  "Python executable on the remote host."
  (let ((python-shell-interpreter "/localhost:/path/to/the/python"))
    (should (s-equals-p "/path/to/the/python" (pythonic-executable)))))

(ert-deftest test-pythonic-executable-virtualenv ()
  "Virtual environment python executable."
  (let ((python-shell-virtualenv-path "/home/me/env"))
    (should (s-equals-p "/home/me/env/bin/python" (pythonic-executable)))))

(ert-deftest test-pythonic-executable-virtualenv-remote ()
  "Virtual environment python executable on the remote host."
  (let ((python-shell-virtualenv-path "/localhost:/vagrant/env"))
    (should (s-equals-p "/vagrant/env/bin/python" (pythonic-executable)))))

(ert-deftest test-pythonic-executable-virtualenv-windows ()
  "Virtual environment python executable on the windows platform."
  (let ((system-type 'windows-nt)
        (python-shell-virtualenv-path "C:/env"))
    (should (s-equals-p "C:/env/Scripts/pythonw" (pythonic-executable)))))

;;; Default directory.

(ert-deftest test-pythonic-default-directory ()
  "Run processes in $HOME by default."
  (should (s-equals-p "~" (pythonic-default-directory))))

(ert-deftest test-pythonic-default-directory-localhost ()
  "Pass directory unmodified in clean environment."
  (should (s-equals-p "/me" (pythonic-default-directory "/me"))))

(ert-deftest test-pythonic-default-directory-interpreter-remote ()
  "Default directory must point to the tramp address in the case
  remote address was specified in the
  `python-shell-interpreter'."
  (let ((python-shell-interpreter "/localhost:/path/to/the/python"))
    (should (s-equals-p "/localhost:~" (pythonic-default-directory)))))

(ert-deftest test-pythonic-default-directory-virtualenv-remote ()
  "Virtual environment `default-directory' on the remote host."
  (let ((python-shell-virtualenv-path "/localhost:/vagrant/env"))
    (should (s-equals-p "/localhost:~" (pythonic-default-directory)))))

;;; Call process.

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

(ert-deftest test-call-pythonic-extra-pythonpaths ()
  "Synchronous python process respect `python-shell-extra-pythonpaths'."
  (let* ((python-shell-extra-pythonpaths '("/home/test/modules")))
    (call-pythonic :buffer "*out2*"
                   :args '("-c" "from __future__ import print_function; import os; print(os.getenv('PYTHONPATH'))"))
    (should (s-equals-p "/home/test/modules\n"
                        (with-current-buffer "*out2*"
                          (buffer-string))))))

(ert-deftest test-call-pythonic-process-environment ()
  "Synchronous python process respect `python-shell-process-environment'."
  (let* ((python-shell-process-environment '("PING=PONG"))
         (process ))
    (call-pythonic :buffer "*out3*"
                   :args '("-c" "from __future__ import print_function; import os; print(os.getenv('PING'))"))
    (should (s-equals-p "PONG\n"
                        (with-current-buffer "*out3*"
                          (buffer-string))))))

;;; Start process.

(ert-deftest test-start-pythonic ()
  "Run asynchronous python process."
  (should (equal '("python" "-V")
                 (process-command
                  (start-pythonic :process "out4"
                                  :buffer "*out4*"
                                  :args '("-V"))))))

(ert-deftest test-start-pythonic-cwd ()
  "Run asynchronous python process with working directory specified."
  (let ((process (start-pythonic :process "out5"
                                 :buffer "*out5*"
                                 :cwd "~"
                                 :args '("-c" "from __future__ import print_function; import os; print(os.getcwd())"))))
    (while (process-live-p process)
      (accept-process-output process))
    (should (s-equals-p (s-concat (expand-file-name "~") "\n\nProcess out5 finished\n")
                        (with-current-buffer "*out5*"
                          (buffer-string))))))

(ert-deftest test-start-pythonic-extra-pythonpaths ()
  "Asynchronous python process respect `python-shell-extra-pythonpaths'."
  (let* ((python-shell-extra-pythonpaths '("/home/test/modules"))
         (process (start-pythonic :process "out6"
                                  :buffer "*out6*"
                                  :args '("-c" "from __future__ import print_function; import os; print(os.getenv('PYTHONPATH'))"))))
    (while (process-live-p process)
      (accept-process-output process))
    (should (s-equals-p "/home/test/modules\n\nProcess out6 finished\n"
                        (with-current-buffer "*out6*"
                          (buffer-string))))))

(ert-deftest test-start-pythonic-process-environment ()
  "Asynchronous python process respect `python-shell-process-environment'."
  (let* ((python-shell-process-environment '("PING=PONG"))
         (process (start-pythonic :process "out7"
                                  :buffer "*out7*"
                                  :args '("-c" "from __future__ import print_function; import os; print(os.getenv('PING'))"))))
    (while (process-live-p process)
      (accept-process-output process))
    (should (s-equals-p "PONG\n\nProcess out7 finished\n"
                        (with-current-buffer "*out7*"
                          (buffer-string))))))

;;; Activate/deactivate environment.

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
