#CLiMP#

This library is experimental. Your comments are very welcome.

##Examples##

###Hello World###

```lisp
(use-package :climp)

(defun hello-world (nthreads)
  (parallel (:number-of-threads nthreads)
    (format t "~&I'm number: ~a" thread-number)
    (once
      (format t "~&Number-of-threads: ~a" number-of-threads))))
```

###From sequential cycle to parallel cycle###

```lisp
(use-package :climp)

(defun sequential-function (niterations)
  (dotimes (i niterations)
    (some-action i)))

(defun parallel-function (niterations)
  (parallel-dotimes (i niterations)
    (some-action i)))
```