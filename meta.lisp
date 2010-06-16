;; Copyright (c) 2009, 2010 Vitaly Mayatskikh <v.mayatskih@gmail.com>
;;
;; This file is part of CL-ZMQ.
;;
;; Vitaly Mayatskikh grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :zeromq)

(defmacro defcfun* (name-and-options return-type &body args)
  (let* ((c-name (car name-and-options))
	 (l-name (cadr name-and-options))
         (n-name (intern (format nil "%~A" l-name) :zeromq))
	 (name (list c-name n-name))

	 (docstring (when (stringp (car args)) (pop args)))
	 (ret (gensym)))
    (loop with opt
       for i in args
       unless (consp i) do (setq opt t)
       else
       collect i into args*
       and if (not opt) collect (car i) into names
       else collect (car i) into opts
       and collect (list (car i) 0) into opts-init
       end
       finally (return
	 `(progn
	    (defcfun ,name ,return-type
	      ,@args*)

	    (defun ,l-name (,@names ,@(when opts-init `(&optional ,@opts-init)))
	      ,docstring
	      (let ((,ret (,n-name ,@names ,@opts)))
		(if ,(if (eq return-type :pointer)
			   `(zerop (pointer-address ,ret))
			   `(not (zerop ,ret)))
		    (let ((errno (errno)))
		      (cond
			#-windows
			((eq errno isys:eagain) (error 'error-again :argument errno))
			(t (error (convert-from-foreign (%strerror errno) :string)))))
		,ret))))))))

;; Stolen from CFFI. Uses custom allocator (alloc-fn) instead of foreign-alloc
(defun copy-lisp-string-octets (string alloc-fn &key (encoding cffi::*default-foreign-encoding*)
                             (null-terminated-p t) (start 0) end)
  "Allocate a foreign string containing Lisp string STRING.
The string must be freed with FOREIGN-STRING-FREE."
  (check-type string string)
  (cffi::with-checked-simple-vector ((string (coerce string 'babel:unicode-string))
				     (start start) (end end))
    (declare (type simple-string string))
    (let* ((mapping (cffi::lookup-mapping cffi::*foreign-string-mappings* encoding))
           (count (funcall (cffi::octet-counter mapping) string start end 0))
           (length (if null-terminated-p
                       (+ count (cffi::null-terminator-len encoding))
                       count))
	   (ptr (funcall alloc-fn length)))
      (funcall (cffi::encoder mapping) string start end ptr 0)
      (when null-terminated-p
        (dotimes (i (cffi::null-terminator-len encoding))
          (setf (mem-ref ptr :char (+ count i)) 0)))
      (values ptr length))))

