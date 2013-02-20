
(in-package :climp)

;;;==============================================================

(defvar *reductions* (make-hash-table)
  "Contain REDUCTION definitions for RARALLEL region.")

(defmacro define-reduction (name args &body body)
"
Define new REDUCTION for PARALLEL region. 
REDUCTION is a function that should return definition of PARALLEL :VARS variable.
"
  `(setf (gethash ,(make-keyword name) *reductions*)
	 (lambda ,args ,@body)))

;;;==============================================================

(defun reduction->vars (reduction-vars)
"
Translate REDUCTION defenitions of REDUCTION-VARS to PARALLEL :VARS defenitions.
"
  (loop :for var :in reduction-vars
	:collect (apply (gethash (first var) *reductions*)
			(rest var))))

;;;==============================================================

(defmacro define-openmp-like-reduction (name operation &optional (start-value 0))
 `(define-reduction ,name (var &optional (start-value ,start-value))
    (with-gensyms (private-var)
      `(,var ,start-value (lambda (,private-var)
			    (setf ,var (,',operation ,var ,private-var)))))))

(define-openmp-like-reduction incf +)
(define-openmp-like-reduction decf -)
(define-openmp-like-reduction mulf * 1)
(define-openmp-like-reduction divf / 1)

(define-openmp-like-reduction min min most-positive-fixnum)
(define-openmp-like-reduction max max most-negative-fixnum)

(define-openmp-like-reduction or or nil)

(define-reduction and (var &optional (start-value t))
  (with-gensyms (private-var)
    `(,var ,start-value (lambda (,private-var)
			  (setf ,var (and ,private-var ,var))))))

;;;==============================================================

(define-reduction list (var &optional start-value)
  (with-gensyms (accum-list private-var)
   `(,var ,start-value (lambda (,private-var)
			 (with-global-vars ((,accum-list (make-list number-of-threads)))
			   (setf (nth thread-number ,accum-list) ,private-var)
			   (last-private
			     (setf ,var ,accum-list)))))))

;;;==============================================================
