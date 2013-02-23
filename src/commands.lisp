
(in-package :climp)

;;;==============================================================

(defmacro master (&body body)
"
Specifies a block that is executed by the master thread of the team.
"
 `(progn ,@body))


(defmacro slave (&body body)
"
Specifies a block that is executed by the slave threads of the team.
"
 `(progn ,@body))

;;;==============================================================
;;; Define a stubs for some PARALLEL commands

(defmacro each-thread (&body body)
"
Specifies a block that is executed by the each thread of the team.
Needed for separation functions and macros from PARALLEL commands.
"
  (declare (ignore body))
  (error "EACH-THREAD can be performed only in PARALLEL region"))


(defmacro critical (&body body)
"
Restricts execution of the associated block to a single thread at a time.
Use (critical :skip t ...) to ignore the critical section if it is already 
blocked.
"
  (declare (ignore body))
  (error "CRITICAL can be performed only in PARALLEL region"))


(defmacro once (&body body)
"
Specifies a block that is executed only once by only one of the 
threads in the team (not necessarily the master thread).
"
  (declare (ignore body))
  (error "ONCE can be performed only in PARALLEL region"))


(defmacro single (&body body)
"
Specifies a block that is executed by only one of the threads in the 
team (not necessarily the master thread).
"
  (declare (ignore body))
  (error "SINGLE can be performed only in PARALLEL region"))


(defun barrier (&optional number-of-threads)
"
Specifies an explicit barrier at the point at which the construct appears.

By default blocks all threads of the team. Can be used to block specified NUMBER-OF-THREADS.
NUMBER-OF-THREADS must be evaluated without PARALLEL region local variables.
"
  (declare (ignore number-of-threads))
  (error "BARRIER can be performed only in PARALLEL region"))


(defmacro with-global-vars (variables &body body)
"
Specifies a new global to PARALLEL region VARIABLES.

Should be used inside DEFINE-REDUCTION. See (define-reduction list ...) as example.
"
  (declare (ignore variables body))
  (error "WITH-GLOBAL-VARS can be performed only in PARALLEL region"))


(defmacro last-private (&body body)
"
Specifies a block that is only executed by the last of the threads in the team.

WARNING! Not thread-safe realization! Should be used inside DEFINE-REDUCTION.
See (define-reduction list ...) as example.
"
  (declare (ignore body))
  (error "LAST-PRIVATE can be performed only in PARALLEL region"))

;;;==============================================================



;;;==============================================================

(defun cut-trees-by-head (head tree &optional break-heads)
"
Cut from TREE every subtree which first element is HEAD.

Can optionaly stop recurcieve evaluation when meet object from BREAK-HEADS.
"
  (cond ((not (listp tree))              tree)
	((eq  (car tree) head)           nil )
	((member (car tree) break-heads) tree)
	(t (loop :for obj :in tree
		 :when (cut-trees-by-head head obj)
		   :collect it 
		 :else :when (null obj)
		   :collect nil))))

;;;==============================================================



;;;==============================================================

(defvar *parallel-commands* (make-hash-table)
  "Contain PARALLEL-COMMANDS defenitions for RARALLEL region.")

(defmacro define-parallel-command (name args &body body)
"
Define new COMMAND for PARALLEL region. 
COMMAND is a function that return list in format: (:vars VARS :body BODY).
  VARS - PARALLEL region global vars.
  BODY - code that substitute COMMAND.
"
  `(setf (gethash ',name *parallel-commands*)
	 (lambda ,args
	   ,@body)))

;;;==============================================================

(defun compile-parallel (body) 
  (let (vars)
    (labels ((%compile-parallel (body)
	       (acond

		 ((atom body) body)
		 
		 ((eq  (first body) 'each-thread) `(progn ,@(rest body)))

		 ((gethash (first body) *parallel-commands*)
		  (let ((definition (apply it (rest body))))
		    (appendf vars (getf definition :vars))
		    (%compile-parallel (getf definition :body))))

		 (t (mapcar #'%compile-parallel body)))))

      (values (%compile-parallel body) vars))))

;;;==============================================================



;;;==============================================================

(define-parallel-command parallel (args &rest body)
 `(:vars nil
   :body (each-thread (parallel ,args ,@body))))

;;;==============================================================

(define-parallel-command critical (&rest body)
  (with-gensyms (lock skip)
    (let ((skip? (eq :skip (first body))))
      `(:vars ((,lock (make-lock))
	       ,@(when skip? 
		  `((,skip ,(second body)))))
	:body (when (acquire-lock ,lock (not ,(when skip? skip)))
		(unwind-protect
		     (progn
		       ,@(funcall (if skip? #'cddr #'identity) body))
		  (release-lock ,lock)))))))

;;;==============================================================

(define-parallel-command once (&rest body)
  (with-gensyms (flag)
    `(:vars ((,flag t))
      :body (when ,flag
	      (critical :skip t
		(setf ,flag nil)
	        ,@body)))))

;;;==============================================================

(define-parallel-command single (&rest body)
  (with-gensyms (thread-number)
    `(:vars ((,thread-number -1))
      :body (flet ((single-function () ,@body))
	      (declare (inline single-function))
	      (if (= ,thread-number thread-number)
		  (single-function)
		  (when (= ,thread-number -1)
		    (once (setf ,thread-number thread-number)
		          (single-function))))))))

;;;==============================================================

(defclass circular-barrier ()
  ((lock-1    :initform (make-lock))
   (lock-2    :initform (make-lock))
   (cv-1      :initform (make-condition-variable))
   (cv-2      :initform (make-condition-variable))
   (odd-iterp :initform t :type boolean)
   (counter-1 :initarg  :counter-1 
	      :type      fixnum
	      :initform (error "Not initialized counter"))
   (counter-2 :initarg  :counter-2 
	      :type      fixnum
	      :initform (error "Not initialized counter"))))

(defun make-barrier (counter)
"
Initialize new CIRCULAR-BARRIER from COUNTER.
"
  (make-instance 'circular-barrier :counter-1 counter
				   :counter-2 counter))

(defgeneric synchronize (barrier))

(defmethod synchronize ((barrier circular-barrier))
  (with-slots (lock-1 cv-1 lock-2 cv-2 counter-1 counter-2 odd-iterp)
      barrier
    
    (if odd-iterp

	(with-lock-held (lock-1)
	  (when (plusp (decf counter-1))
	    (condition-wait cv-1 lock-1))
	  (incf counter-1)
	  (setf odd-iterp nil)
	  (condition-notify cv-1))

	(with-lock-held (lock-2)
	  (when (plusp (decf counter-2))
	    (condition-wait cv-2 lock-2))
	  (incf counter-2)
	  (setf odd-iterp t)
	  (condition-notify cv-2)))))

(define-parallel-command barrier (&optional (number-of-threads 'number-of-threads))
  (with-gensyms (barrier)
   `(:vars ((,barrier (make-barrier ,number-of-threads)))
     :body (synchronize ,barrier))))

;;;==============================================================

(define-parallel-command with-global-vars (vars &rest body)
 `(:vars ,vars
   :body (progn ,@body)))

;;;==============================================================

(define-parallel-command last-private (&rest body)
  (with-gensyms (counter)
   `(:vars ((,counter number-of-threads))
     :body (when (zerop (decf ,counter))
	     ,@body))))

;;;==============================================================
