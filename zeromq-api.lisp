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
  (require :defsubst))

(defcfun ("memcpy" memcpy) :pointer
  (dst	:pointer)
  (src	:pointer)
  (len	:long))

(defclass msg ()
  ((raw		:accessor msg-raw :initform nil)))

(defmethod initialize-instance :after ((msg msg) &key size data)
  (let ((obj (ff:allocate-fobject '%msg :c)))
    (setf (msg-raw msg) obj)
    (excl:schedule-finalization msg
                                (lambda (msg)
                                  (%msg-close (msg-raw msg))
                                  (ff:free-fobject (msg-raw msg))))
    (cond
      (size
       (%msg-init-size obj size))
      (data
       (etypecase data
         (string
          (%msg-init-size obj (length data))
          (excl:string-to-native data :address (%msg-data obj)))
         #+(or)
         ((simple-array (unsigned-byte 8))
          (let ((len (length data)))
            (%msg-init-size obj len)
            (with-pointer-to-vector-data (ptr data)
              (memcpy (%msg-data obj) ptr len))))
         #+(or)
         (array (progn
                  (%msg-init-size obj (length data))
                  (let ((ptr (%msg-data obj))
                        (i -1))
                    (map nil (lambda (x)
                               (setf (mem-aref ptr :uchar (incf i)) x))
                         data))))))
	  (t (msg-init obj)))))

(excl::defsubst msg-init-size (msg size)
  (%msg-init-size (msg-raw msg) size))

(excl::defsubst msg-close (msg)
  (%msg-close (msg-raw msg)))

(excl::defsubst msg-size (msg)
  (%msg-size (msg-raw msg)))

(excl::defsubst msg-move (dst src)
  (%msg-move (msg-raw dst) (msg-raw src)))

(excl::defsubst msg-copy (dst src)
  (%msg-copy (msg-raw dst) (msg-raw src)))

(excl::defsubst msg-data-as-is (msg)
  (%msg-data (msg-raw msg)))

(defun msg-data-as-string (msg)
  (let ((data (%msg-data (msg-raw msg))))
    (unless (zerop data)
      (excl:native-to-string data))))

(defun msg-data-as-array (msg &optional array)
  (let ((data (%msg-data (msg-raw msg))))
    (unless (zerop data)
      (let* ((len (msg-size msg))
	     (array (or array (make-array len :element-type '(unsigned-byte 8)))))
        (dotimes (i len)
          (setf (aref array i) (sys:memref-int data 0 i :unsigned-byte)))
        array))))

(defclass pollitem ()
  ((raw		:accessor pollitem-raw :initform nil)
   (socket	:accessor pollitem-socket :initform nil :initarg :socket)
   (fd		:accessor pollitem-fd :initform -1 :initarg :fd)
   (events	:accessor pollitem-events :initform 0 :initarg :events)
   (revents	:accessor pollitem-revents :initform 0)))

(defmethod initialize-instance :after ((pollitem pollitem) &key)
  (let ((obj (excl:aclmalloc '%pollitem)))
    (setf (pollitem-raw pollitem) obj)
    (excl:schedule-finalization pollitem
                                (lambda (pollitem)
                                  (excl:aclfree (pollitem-raw pollitem))))))

(defun bind (s address)
  (with-foreign-string (addr address)
    (%bind s addr)))

(defun connect (s address)
  (with-foreign-string (addr address)
    (%connect s addr)))

(defmacro with-context ((context io-threads) &body body)
  `(let ((,context (init ,io-threads)))
     (unwind-protect
	  (progn ,@body)
       (term ,context))))

(defmacro with-socket ((socket context type) &body body)
  `(let ((,socket (socket ,context ,type)))
     (unwind-protect
	  (progn ,@body)
       (close ,socket))))

(defun send (s msg &optional flags)
  (%send s (msg-raw msg) (or flags 0)))

(defun recv (s msg &optional flags)
  (%recv s (msg-raw msg) (or flags 0)))

(defun setsockopt (socket option value)
  (etypecase value
    (string
     (with-foreign-string (string value)
       (%setsockopt socket option string (length value))))
    (integer
     (ff:with-stack-fobject (int :long)
       (setf (ff:fslot-value int) value)
       (%setsockopt socket option int (ff:sizeof-fobject :long))))))

(defun getsockopt (socket option)
  (ff:with-static-fobjects ((opt :long)
                            (len :long))
    (setf (ff:fslot-value opt) 0
	  (ff:fslot-value len) (ff:sizeof-fobject :long))
    (%getsockopt socket option opt len)
    (ff:fslot-value opt :long)))

(defun poll (items &optional (timeout -1))
  (let ((len (length items)))
    (ff:with-static-fobject (%items `(:array %pollitem ,len))
      (dotimes (i len)
	(let ((item (nth i items))
	      (%item (ff:fslot-value %items i)))
	  (with-foreign-slots ((socket fd events revents) %item %pollitem)
	    (setf socket (pollitem-socket item)
		  fd (pollitem-fd item)
		  events (pollitem-events item)))))
      (when (plusp (%poll %items len timeout))
        (loop for i below len
           for revent = (ff:fslot-value %items i 'revents)
           collect (setf (pollitem-revents (nth i items)) revent))))))

(defmacro with-polls (list &body body)
  `(let ,(loop for (name . polls) in list
	    collect `(,name
		      (list
		       ,@(loop for (socket . events) in polls
			    collect `(make-instance 'pollitem
						    :socket ,socket
						    :events ,events)))))
     ,@body))

(defun version ()
  (ff:with-stack-fobjects ((major :int)
                           (minor :int)
                           (patch :int))
    (%version major minor patch)
    (format nil "~d.~d.~d"
	    (ff:fslot-value major)
	    (ff:fslot-value minor)
	    (ff:fslot-value patch))))

;
