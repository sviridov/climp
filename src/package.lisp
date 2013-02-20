
(defpackage #:climp

  (:use #:cl
	#:bt 
	#:cffi
	#:alexandria
	#:anaphora)

  (:export #:number-of-processors
	   #:*number-of-threads-function*

	   #:*thread-pool*
	   #:check
	   #:submit
	   #:receive
	   
	   #:parallel
	   #:parallel-for
	   #:parallel-dotimes
	   #:parallel-dolist

	   #:number-of-threads
	   #:thread-number
	   #:each-thread
	   #:critical
	   #:barrier
	   #:single
	   #:master
	   #:slave
	   #:once

	   #:define-parallel-command
	   #:define-reduction
	   #:with-global-vars
	   #:last-private))
