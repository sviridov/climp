
(in-package :climp)

;;;==============================================================

(defmacro define-parallel-macro (name args &body body)
"
Macro that simplify definitions of PARALLEL, PARALLEL-FOR, PARALLEL-DOTIMES and PARALLEL-DOLIST.
"
 `(defmacro ,name ((,@args (number-of-threads '(funcall *number-of-threads-function*))
			   (wait t)
			   (with-master t)
			   vars) &body body)
    (declare (ignorable number-of-threads wait with-master vars))
    ,@body))

;;;==============================================================

(define-parallel-macro parallel (&key)
  
  (with-gensyms (finally-actions)
						   
    (let* ((reductionp (compose #'keywordp #'first #'ensure-list))
           (reductions (reduction->vars (remove-if-not reductionp vars)))
           (simple-vars (remove-if reductionp vars))
           (vars (mapcar #'ensure-list (append simple-vars reductions))))

     `(let ((number-of-threads ,number-of-threads))
	(declare (type (integer 1 1024) number-of-threads))

       ,(multiple-value-bind (body global-vars)
	    (compile-parallel

	     `(let* ((,finally-actions (list ,@(mapcar #'third vars)))
			      
		     ,@(loop :for var :in vars
			     :collect (list (first var) (second var))))

		(declare (ignorable ,finally-actions))
		,@body
		,@(when (some #'identity (mapcar #'third vars))
		    `((critical 
			(loop :for  var :in (list ,@(mapcar #'first vars))
			      :for  act :in ,finally-actions
			      :when act :do (funcall act var)))))))
	    
	  (let* ((break-heads  '(master slave each-thread))
		 (master-body   (cut-trees-by-head 'slave  body break-heads))
		 (slave-body    (cut-trees-by-head 'master body break-heads)))

	    `(let* ,global-vars
	       (parallel-impl number-of-threads ,wait ,with-master
			 
			      (lambda ()
                                (let ((thread-number 0))
                                  (declare (type bit  thread-number)
                                           (ignorable thread-number)) 
                                  ,master-body))

			      (lambda (thread-number)
				(declare (type (integer 0 1024) thread-number)
					 (ignorable thread-number))
				(lambda () ,slave-body))))))))))

;;;==============================================================

(defun parallel-impl (number-of-threads wait 
		      with-master
		      master-handler slave-handler)

  (let ((result-hook (submit *thread-pool*
		      (mapcar slave-handler (if  with-master
						(iota (1- number-of-threads) :start 1)
						(iota number-of-threads))))))

    (when with-master
      (funcall master-handler))

    (when (and wait with-master)
      (receive *thread-pool* result-hook))

    result-hook))      

;;;==============================================================
