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

(defmacro defcfun ((c-name lisp-name) return-type &rest arguments)
  `(ff:def-foreign-call (,lisp-name ,c-name)
       ,(mapcar #'map-argument arguments)
     :returning (,(map-type return-type))))

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

(defmacro defcstruct (name &rest args)
  `(ff:def-foreign-type ,name (:struct ,@(mapcar #'map-argument args))))

(defmacro defcallback (name return-type args &body body)
  (assert (eq :void return-type))
  `(ff:defun-foreign-callable ,name ,(mapcar #'map-argument args)
     ,@body))

(defmacro with-foreign-objects (objects &body body)
  `(ff:with-static-fobjects ,(mapcar #'map-argument objects)
     ,@body))

(defmacro mem-aref (object type &rest indices)
  `(ff:fslot-value-typed ,(map-type type) :c ,object ,@indices))

(defmacro foreign-type-size (type)
  `(ff:sizeof-fobject ,(map-type type)))

(defmacro foreign-alloc (type)
  `(ff:allocate-fobject ,type))

