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

(define-condition error-again (error)
  ())

#+allegro
(defun map-type (type)
  (case type
    (:pointer
     '(* :void))
    #+64bit
    (:int64
     :long)
    (:uchar
     :unsigned-char)
    (otherwise
     (cond
       ((keywordp type)
        type)
       ((symbolp type)
        `(* ,type))
       ((listp type)
        (assert (= :pointer (first type)))
        `(* ,(map-type (second type))))))))

#+allegro
(defun map-argument (argument)
  (destructuring-bind (name type &rest args) argument
    (list name
          (cond
            ((eq :count (first args))
             `(:array ,(map-type type) ,(second args)))
            ((eq :pointer (first args))
             `(* ,(or (second args) :void)))
            (t
             (map-type type))))))

#+allegro
(defmacro defcfun ((c-name lisp-name) return-type &rest arguments)
  `(ff:def-foreign-call (,lisp-name ,c-name)
       ,(mapcar #'map-argument arguments)
     :returning (,(map-type return-type))))

#+allegro
(defmacro defcfun* ((c-name lisp-name) return-type &rest arguments)
  (let ((lisp-stub-name (intern (format nil "%~A" lisp-name) :zeromq))
        (argument-names (mapcar #'first arguments))
        (return-value (gensym)))
    `(progn
       (ff:def-foreign-call (,lisp-stub-name ,c-name)
           ,(mapcar #'map-argument arguments)
         :returning (,(map-type return-type))
         :error-value :errno)
       (defun ,lisp-name ,argument-names
         (multiple-value-bind (,return-value errno) (,lisp-stub-name ,@argument-names)
             (if ,(if (eq return-type :pointer)
                  `(zerop (ff:foreign-pointer-address ,return-value))
                  `(not (zerop ,return-value)))
                 (if (eq errno excl::*eagain*)
                     (error 'error-again)
                     (error 'syscall-error :errno errno))
                 ,return-value))))))

#+allegro
(defmacro defcstruct (name &rest args)
  `(ff:def-foreign-type ,name (:struct ,@(mapcar #'map-argument args))))

#+allegro
(defmacro defcallback (name return-type args &body body)
  (assert (eq :void return-type))
  `(ff:defun-foreign-callable ,name ,(mapcar #'map-argument args)
     ,@body))

#+allegro
(defmacro with-foreign-objects (objects &body body)
  `(ff:with-static-fobjects ,(mapcar #'map-argument objects)
     ,@body))

#+allegro
(defmacro mem-aref (object type &rest indices)
  `(ff:fslot-value-typed ,(map-type type) :c ,object ,@indices))

#+allegro
(defmacro foreign-type-size (type)
  `(ff:sizeof-fobject ,(map-type type)))

#-allegro
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
