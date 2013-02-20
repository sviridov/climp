
(in-package :climp)

;;;==============================================================

(define-parallel-macro parallel-dolist (var list &rest args &key)
"
Parallel version of DOLIST. Supports every PARALLEL keys and commands.
"
 `(parallel ,args
    (parallel-dolist (,var ,list)
      ,@body)))

(define-parallel-command parallel-dolist (args &rest body)
  (with-gensyms (list-place list-sym)
    (destructuring-bind (var list) args
      `(:vars ((,list-place ,list))
	:body (do ((,list-sym (nthcdr thread-number ,list-place)
			      (nthcdr number-of-threads ,list-sym)))
		  ((null ,list-sym))
		(declare (type list ,list-sym))
		(let ((,var (car ,list-sym)))
		  ,@body))))))		  

;;;==============================================================
