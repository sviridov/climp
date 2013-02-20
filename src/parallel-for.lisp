
(in-package :climp)

;;;==============================================================

(define-parallel-macro parallel-for (var start stop &rest args
					            &key (by 1))
"
Parallel cycle for VAR from START to STOP by BY.
Supports every PARALLEL keys and commands.
"
 `(parallel ,(remove-from-plist args :by)
    (parallel-for (,var ,start ,stop :by ,by)
      ,@body)))

(define-parallel-command parallel-for (args &rest body)
  (with-gensyms (start-sym stop-sym by-sym
		 by*thread-number by*number-of-threads)
    (destructuring-bind (var start stop &key (by 1)) args
     `(:vars ((,start-sym ,start)
	      (,stop-sym  ,stop)
	      (,by-sym    ,by)
	      (,by*number-of-threads (* ,by-sym number-of-threads)))
       :body (let ((,by*thread-number (* ,by-sym thread-number)))
	       (if (< ,start-sym ,stop-sym)
		   (do ((,var (+ ,start-sym ,by*thread-number) 
			      (+ ,var ,by*number-of-threads)))
		       ((> ,var ,stop-sym))
		     ,@body)
		   (do ((,var (+ ,start-sym ,by*thread-number) 
			      (+ ,var ,by*number-of-threads)))
		       ((< ,var ,stop-sym))
		     ,@body)))))))

;;;==============================================================

(define-parallel-macro parallel-dotimes (var times &rest args &key)
"
Parallel version of DOTIMES. Supports every PARALLEL keys and commands.
"
 `(parallel-for (,var 0 (1- ,times) ,@args)
   ,@body))

(define-parallel-command parallel-dotimes (args &rest body)
  (destructuring-bind (var times) args
   `(:vars ()
     :body (parallel-for (,var 0 (1- ,times))
	     ,@body))))

;;;==============================================================
