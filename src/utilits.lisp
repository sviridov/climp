
(in-package :climp)

(proclaim '(inline number-of-processors))

;;;==============================================================

#+(or linux freebsd)
(progn
  
  (defcfun "sysconf" :long
    (code :int))
  
  (setf (documentation 'sysconf 'function)
	"C function from <unistd.h>.")

  (defun number-of-processors ()
    (sysconf #+linux 84 #+freebsd 54))) ; 84 (54) - _SC_NPROCESSORS_ONLN

;;;==============================================================

#+(or win32 win64)
(progn
  
  (defctype dword :unsigned-long)

  (defcstruct (system-info :size 32) 
    (number-of-processors dword :offset 20))
  
  (defcfun ("GetSystemInfo" %get-system-info :convention :stdcall) :void
    (system-info :pointer))

  (setf (documentation '%get-system-info 'function)
	"C function from <windows.h>.")
  
  (defun number-of-processors ()
    (with-foreign-object (si 'system-info)
      (%get-system-info si)
      (foreign-slot-value si 'system-info 'number-of-processors))))

;;;==============================================================

#+(or linux freebsd win32 win64)
(setf (documentation 'number-of-processors 'function)
"
Return number of processors (cores) on current computer.
Not tested on FreeBSD yet!
Realized only for: Linux, FreeBSD, Windows.
")

#+freebsd
(warn 
"
Function CLIMP:NUMBER-OF-PROCESSORS not tested for FreeBSD yet!
Please test it and feedback the results.
")

#-(or linux freebsd win32 win64)
(warn
"
Function CLIMP:NUMBER-OF-PROCESSORS not realized for your operation system yet!
Default NUMBER-OF-THREADS for PARALLEL region set to 4.
")

;;;==============================================================

(defvar *number-of-threads-function* 
  #+(or linux freebsd win32 win64) #'number-of-processors
  #-(or linux freebsd win32 win64) (lambda () 4)

  "Return default NUMBER-OF-THREADS for PARALLEL region.")

;;;==============================================================
