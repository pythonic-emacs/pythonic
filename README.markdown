# Pythonic [![MELPA](http://www.melpa.org/packages/pythonic-badge.svg)](http://www.melpa.org/#/pythonic)     [![MELPA Stable](https://stable.melpa.org/packages/pythonic-badge.svg)](https://stable.melpa.org/#/pythonic)

Utility functions for writing pythonic emacs package.

## Installation

You can install this package form [Melpa](http://melpa.org)

    M-x package-install RET pythonic RET

## Usage

This library provide function for convenient running python on
different platforms on local and remote hosts including Docker
containers and Vagrant virtual machines.  To use `pythonic` with
Docker you need to install
[docker-tramp](https://github.com/emacs-pe/docker-tramp.el) Emacs
package.

You can use remote interpreter from the tramp buffer.

```lisp
(cd "/ssh:user@host:/home/user/")
;; or
(cd "/docker:root@container:/root/")
```

## Functions

### pythonic-call-process

Pythonic wrapper around `call-process`.

FILE is the input file.  BUFFER is the output destination.  DISPLAY
specifies to redisplay BUFFER on new output.  ARGS is the list of
arguments passed to `call-process`.  CWD will be working directory for
running process.

```lisp
(pythonic-call-process :buffer "*Pythonic*"
                       :args '("-V")
                       :cwd "~")
```

### pythonic-start-process

Pythonic wrapper around `start-process`.

PROCESS is a name of the created process.  BUFFER is a output
destination. ARGS are the list of args passed to `start-process`.  CWD
will be working directory for running process.  FILTER must be a
symbol of process filter function if necessary.  SENTINEL must be a
symbol of process sentinel function if necessary.  QUERY-ON-EXIT will
be corresponding process flag.

```lisp
(pythonic-start-process :process "pythonic"
                        :buffer "*Pythonic*"
                        :args '("-c" "print('PING')")
                        :cwd "~"
                        :filter (lambda (process output) (message output))
                        :sentinel (lambda (process event) (message "Done."))
                        :query-on-exit nil)
```

### pythonic-remote-p

Determine remote or local virtual environment.

```lisp
(pythonic-remote-p)
```

### pythonic-remote-docker-p

Determine docker remote virtual environment.

```lisp
(pythonic-remote-docker-p)
```

### pythonic-remote-vagrant-p

Determine vagrant remote virtual environment.

```lisp
(pythonic-remote-vagrant-p)
```

### pythonic-remote-user

Get user of the connection to the remote python interpreter.

```lisp
(pythonic-remote-user)
```

### pythonic-remote-host

Get host of the connection to the remote python interpreter.

```lisp
(pythonic-remote-host)
```

### pythonic-remote-port

Get port of the connection to the remote python interpreter.

```lisp
(pythonic-remote-port)
```

## Commands

### pythonic-activate

Activate python virtual environment.  Tramp paths are supported.

### pythonic-deactivate

Deactivate python virtual environment.
