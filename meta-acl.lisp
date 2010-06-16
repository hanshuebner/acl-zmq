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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq ff:*pass-structs-by-value* nil))

(defun map-type (type &rest args)
  (let ((type (if (and (listp type)
                       (eq 'quote (first type)))
                  (second type)
                  type)))
    (cond
      ((eq type :pointer)
       `(* ,(or (first args) :void)))
      ((listp type)
       (assert (eql :pointer (first type)) () "expected :pointer, got ~A" (first type))
       `(* ,(map-type (second type))))
      (t
       (case type
         #+64bit
         (:int64
          :long)
         (:uchar
          :unsigned-char)
         (t
          type))))))

(defun map-argument (argument)
  (destructuring-bind (name type &rest args) argument
    (list name
          (cond
            ((eq :count (first args))
             `(:array ,(map-type type) ,(second args)))
            ((eq :pointer (first args))
             `(* ,(or (second args) :void)))
            (t
             (apply #'map-type type args))))))

(defmacro defcfun ((c-name lisp-name) return-type &rest arguments)
  `(ff:def-foreign-call (,lisp-name ,c-name)
       ,(mapcar #'map-argument arguments)
     :returning (,(map-type return-type))
     :strings-convert nil))

(defmacro defcfun* ((c-name lisp-name) return-type &rest arguments)
  (let ((lisp-stub-name (intern (format nil "%~A" lisp-name) :zeromq))
        (argument-names (mapcar #'first arguments))
        (return-value (gensym)))
    `(progn
       (ff:def-foreign-call (,lisp-stub-name ,c-name)
           ,(mapcar #'map-argument arguments)
         :returning (,(map-type return-type))
         :strings-convert nil
         :error-value :errno)
       (defun ,lisp-name ,argument-names
         (multiple-value-bind (,return-value errno) (,lisp-stub-name ,@argument-names)
           (if ,(if (eq return-type :pointer)
                    `(zerop ,return-value)
                    `(not (zerop ,return-value)))
               (if (eq errno excl::*eagain*)
                   (error 'error-again)
                   (error 'zmq-syscall-error :call-name ,c-name :errno errno))
               ,return-value))))))

(defmacro defcstruct (name &rest args)
  `(ff:def-foreign-type ,name (:struct ,@(mapcar #'map-argument args))))

(defmacro defcallback (name return-type args &body body)
  (assert (eq :void return-type))
  `(ff:defun-foreign-callable ,name ,(mapcar #'map-argument args)
     ,@body))

(defmacro with-foreign-string ((c-string lisp-string) &body body)
  `(let ((,c-string (excl:string-to-native ,lisp-string)))
     (unwind-protect
          (progn ,@body)
       (excl:aclfree ,c-string))))

(defmacro with-foreign-slots ((slots address type) &body body)
  (flet
      ((gen-slot-accessor (slot-name)
         `(,slot-name (ff:fslot-value-typed ',type :c ,address ',slot-name))))
    `(symbol-macrolet ,(mapcar #'gen-slot-accessor slots)
       ,@body)))