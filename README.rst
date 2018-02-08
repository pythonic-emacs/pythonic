
.. |melpa| image:: http://melpa.org/packages/pythonic-badge.svg
    :target: http://melpa.org/#/pythonic
    :alt: Melpa

========
Pythonic
========

|melpa|

Utility functions for writing pythonic emacs package.

Installation
------------

You can install this package form Melpa_::

    M-x package-install RET pythonic RET

Usage
-----

This library provide function for convenient running python on
different platforms on local and remote hosts including Docker
containers.  To use ``pythonic`` with Docker you need to install
`docker-tramp`_ Emacs package.

You can use remote interpreter

.. code:: lisp

    (setq python-shell-interpreter "/ssh:user@host:/usr/bin/python")
    ;; or
    (setq python-shell-interpreter "/docker:root@container:/usr/bin/python")

You can use remote virtual environment

.. code:: lisp

    (setq python-shell-virtualenv-root "/ssh:user@host:env")
    ;; or
    (setq python-shell-virtualenv-root "/docker:root@container:/src/app/env")

``python-shell-exec-path`` and ``python-shell-process-environment``
will be translated to remote host too.

Functions
---------

call-pythonic
~~~~~~~~~~~~~

Pythonic wrapper around ``call-process``.

FILE is the input file.  BUFFER is the output destination.  DISPLAY
specifies to redisplay BUFFER on new output.  ARGS is the list of
arguments passed to ``call-process``.  CWD will be working directory
for running process.

.. code:: lisp

    (call-pythonic :buffer "*Pythonic*"
                   :args '("-V")
                   :cwd "~")

start-pythonic
~~~~~~~~~~~~~~

Pythonic wrapper around ``start-process``.

PROCESS is a name of the created process.  BUFFER is a output
destination. ARGS are the list of args passed to ``start-process``.
CWD will be working directory for running process.  FILTER must be a
symbol of process filter function if necessary.  SENTINEL must be a
symbol of process sentinel function if necessary.  QUERY-ON-EXIT will
be corresponding process flag.

.. code:: lisp

    (start-pythonic :process "pythonic"
                    :buffer "*Pythonic*"
                    :args '("-c" "print('PING')")
                    :cwd "~"
                    :filter (lambda (process output) (message output))
                    :sentinel (lambda (process event) (message "Done."))
                    :query-on-exit nil)

pythonic-proper-environment-p
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Determine if python environment has been changed since PROCESS was started.

.. code:: lisp

    (pythonic-proper-environment-p
     (start-pythonic
      :process "pythonic"
      :args '("-V")))

pythonic-remote-p
~~~~~~~~~~~~~~~~~

Determine remote or local virtual environment.

.. code:: lisp

    (pythonic-remote-p)

pythonic-remote-docker-p
~~~~~~~~~~~~~~~~~~~~~~~~

Determine docker remote virtual environment.

.. code:: lisp

    (pythonic-remote-docker-p)

pythonic-remote-vagrant-p
~~~~~~~~~~~~~~~~~~~~~~~~~

Determine vagrant remote virtual environment.

.. code:: lisp

    (pythonic-remote-vagrant-p)

pythonic-remote-user
~~~~~~~~~~~~~~~~~~~~

Get user of the connection to the remote python interpreter.

.. code:: lisp

    (pythonic-remote-user)

pythonic-remote-host
~~~~~~~~~~~~~~~~~~~~

Get host of the connection to the remote python interpreter.

.. code:: lisp

    (pythonic-remote-host)

pythonic-remote-port
~~~~~~~~~~~~~~~~~~~~

Get port of the connection to the remote python interpreter.

.. code:: lisp

    (pythonic-remote-port)

Commands
--------

pythonic-activate
~~~~~~~~~~~~~~~~~

Activate python virtual environment.  Tramp paths are supported.

pythonic-deactivate
~~~~~~~~~~~~~~~~~~~

Deactivate python virtual environment.

.. _Melpa: http://melpa.org
.. _docker-tramp: https://github.com/emacs-pe/docker-tramp.el
