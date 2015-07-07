.. |travis| image:: https://travis-ci.org/proofit404/pythonic.png
    :target: https://travis-ci.org/proofit404/pythonic
    :alt: Build Status

.. |coveralls| image:: https://coveralls.io/repos/proofit404/pythonic/badge.png
    :target: https://coveralls.io/r/proofit404/pythonic
    :alt: Coverage Status

.. |melpa| image:: http://melpa.org/packages/pythonic-badge.svg
    :target: http://melpa.org/#/pythonic
    :alt: Melpa

========
Pythonic
========

|travis| |coveralls| |melpa|

Utility functions for writing pythonic emacs package.

Installation
------------

You can install this package form Melpa_::

    M-x package-install RET pythonic RET

Usage
-----

This library provide function for convenient running python on
different platforms on local and remote hosts.

You can use remote interpreter

.. code:: lisp

    (setq python-shell-interpreter "/ssh:user@host:/usr/bin/python")

You can use remote virtual environment

.. code:: lisp

    (setq python-shell-virtualenv-root "/ssh:user@host:env")

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

start-pythonic
~~~~~~~~~~~~~~

Pythonic wrapper around ``start-process``.

PROCESS is a name of the created process.  BUFFER is a output
destination. ARGS are the list of args passed to ``start-process``.
CWD will be working directory for running process.  FILTER must be a
symbol of process filter function if necessary.  SENTINEL must be a
symbol of process sentinel function if necessary.  QUERY-ON-EXIT will
be corresponding process flag.

Commands
--------

pythonic-activate
~~~~~~~~~~~~~~~~~

Activate python virtual environment.

pythonic-deactivate
~~~~~~~~~~~~~~~~~~~

Deactivate python virtual environment.

.. _Melpa: http://melpa.org
