;;; pythonic-test.el --- pythonic test suite

;;; Commentary:

;;; Code:

(require 'ert)
(require 'pythonic)


;;; Defaults.

(ert-deftest test-pythonic-command ()
  "Check basic python command."
  (should (s-equals-p "python" (pythonic-command))))

(ert-deftest test-pythonic-args ()
  "Check basic python args."
  (should (equal '("-V") (pythonic-args "-V"))))

;;; TODO: /ssh:root@localhost#46/path/to/env
;;; TODO: /ssh:root@localhost#46/path/to/env and
;;; `tramp-default-method' doesn't set to ssh, plink, scp, pscp.
;;; TODO: `tramp-default-user' and `tramp-default-host' when
;;; /ssh::/path/to/env was specified.


;;; Interpreter.

(ert-deftest test-pythonic-command-interpreter ()
  "Check interpreter python command on the local host."
  (let ((python-shell-interpreter "/path/to/the/python"))
    (should (s-equals-p "/path/to/the/python" (pythonic-command)))))

(ert-deftest test-pythonic-args-interpreter-remote ()
  "Check interpreter python args on the remote host."
  (let ((python-shell-interpreter "/localhost:/vagrant/env/bin/python"))
    (should (equal '("/vagrant/env/bin/python" "-V")
                   (pythonic-args "-V")))))


;;; Process environment.


;;; PYTHONPATH.

(ert-deftest test-pythonic-args-extra-pythonpaths ()
  "Check PYTHONPATH on the remote host."
  (let ((python-shell-interpreter "/localhost:python")
        (python-shell-extra-pythonpaths '("/home/me/one" "/home/me/two")))
    (should (equal '("env" "PYTHONPATH=/home/me/one:/home/me/two" "python" "-V")
                   (pythonic-args "-V")))))


;;; Exec path.


;;; Virtual environment.

(ert-deftest test-pythonic-command-virtualenv ()
  "Check virtual env python command."
  (let ((python-shell-virtualenv-path "/home/me/env"))
    (should (s-equals-p "/home/me/env/bin/python" (pythonic-command)))))

(ert-deftest test-pythonic-command-virtualenv-windows ()
  "Check virtual env python command on the windows platform."
  (let ((system-type 'windows-nt)
        (python-shell-virtualenv-path "C:/env"))
    (should (s-equals-p "C:/env/Scripts/pythonw" (pythonic-command)))))

(ert-deftest test-pythonic-command-virtualenv-remote ()
  "Check virtual env python command on the remote host."
  (let ((python-shell-virtualenv-path "/localhost:/vagrant/env"))
    (should (s-equals-p "ssh" (pythonic-command)))))

(ert-deftest test-pythonic-command-interpreter-remote ()
  "Check interpreter python command on the remote host."
  (let ((python-shell-interpreter "/localhost:/vagrant/env/bin/python"))
    (should (s-equals-p "ssh" (pythonic-command)))))

(ert-deftest test-pythonic-args-virtualenv-remote ()
  "Check virtual env python args on the remote host."
  (let ((python-shell-virtualenv-path "/localhost:/vagrant/env"))
    (should (equal '("/vagrant/env/bin/python" "-V")
                   (pythonic-args "-V")))))


;;; Processes.

(ert-deftest test-call-pythonic ()
  "Check we can run synchronous python process."
  (should (eq 0 (call-pythonic :buffer "*out*"
                               :args '("-V")))))

(ert-deftest test-call-pythonic-cwd ()
  "Pass current working directory to the process."
  (call-pythonic :buffer "*out1*"
                 :cwd "~"
                 :args '("-c" "import os; print(os.getcwd())"))
  (should (s-equals-p (s-concat (expand-file-name "~") "\n")
                      (with-current-buffer "*out1*"
                        (buffer-string)))))

(ert-deftest test-start-pythonic ()
  "Check we can run asynchronous python process."
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
                                 :args '("-c" "import os; print(os.getcwd())"))))
    (while (process-live-p process)
      (accept-process-output process))
    (should (s-equals-p (s-concat (expand-file-name "~") "\n\nProcess out3 finished\n")
                        (with-current-buffer "*out3*"
                          (buffer-string))))))


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
