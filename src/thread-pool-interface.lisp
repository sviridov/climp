
(in-package :climp)

;;;==============================================================

(defvar *thread-pool* nil 
"
Thread pool for PARALLEL region computations.
NIL means that parallel computations will be performed by using Bordeaux Threads.
")

;;;==============================================================

(defgeneric submit (thread-pool tasks)
  (:documentation
"
Use this method to submit TASKS, which are functions, to *THREAD-POOL*.
SUBMIT should return something that you can use to receive results
from *THREAD-POOL* by calling RECEIVE or CHECK methods.
"))


(defgeneric receive (thread-pool hooks)
  (:documentation
"
Use this method to receive results from *THREAD-POOL*. HOOKS is something 
returned by SUBMIT. RECEIVE should be blocking operation.
"))


(defgeneric check (thread-pool hooks)
  (:documentation
"
Use this method as a non-blocking version of RECEIVE.
"))

;;;==============================================================




;;;==============================================================

(defmethod submit ((thread-pool null) tasks)
  (declare (ignore thread-pool))
  (mapcar #'make-thread tasks))

(defmethod receive ((thread-pool null) threads)
  (declare (ignore thread-pool))
  (mapcar #'join-thread threads))

(defmethod check ((thread-pool null) threads)
  (declare (ignore thread-pool))
  (unless (some #'thread-alive-p threads)
    (mapcar #'join-thread threads)))

;;;==============================================================
