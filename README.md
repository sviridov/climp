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

###Private variables###

```lisp
(use-package :climp)

(defun parallel-count-if-for-list (pred list nthreads)
  (let ((count 0))
    (parallel-dolist (obj list :number-of-threads nthreads
                               :vars ((private-count 0 (lambda (private-count)
                                                         (incf count private-count)))))
      (when (funcall pred obj)
        (incf private-count)))
    count))
```

###Sum of array elements using a variable reduction###

```lisp
(use-package :climp)

(defun array-elements-sum (array nthreads)
  (let ((sum 0))
    (parallel-dotimes (i (array-total-size array) :number-of-threads nthreads
                                                  :vars ((:incf sum)))
      (declare (type fixnum i sum))
      (incf sum (aref array i)))
    sum))
```


###Defining of your own reductions###

```lisp
(use-package :climp)

(define-reduction print (var &optional start-value )
 `(,var ,start-value #'print))
 
(parallel (:number-of-threads 4
           :vars ((:print test "Master thread")))
  (slave
    (setf test "Slave thread")))
```
