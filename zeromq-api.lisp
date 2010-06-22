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
  `(let ((,socket (zmq_socket ,context ,type)))
     (unwind-protect
	  (progn ,@body)
       (zmq_close ,socket))))

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

#+(or)
(do-polling
 ((:fd 0 zmq:pollin)
  (handle-standard-input))
 ((:socket the-zmq-socket zmq:+pollin+)
  (handle-socket the-zmq-socket))
 ((:timeout 30)
  (do-something-periodically))
 ((:always)
  (do-something-after-every-poll)))

(defun parse-poll-clauses (clauses)
  (let (pollitems timeout always)
    (dolist (clause clauses)
      (destructuring-bind ((directive &optional arg event) &body body) clause
        (ecase directive
          (:fd (push `((:fd ,arg :event ,event) (lambda () ,@body)) pollitems))
          (:socket (push `((:socket ,arg :event ,event) (lambda () ,@body)) pollitems))
          (:timeout (when timeout
                      (error "duplicate :timeout clause in do-polling"))
                    (setf timeout (list arg `(lambda () ,@body))))
          (:always (setf always `(lambda () ,@body))))))
    (list (nreverse pollitems) timeout always)))

(defun init-pollitems (pollitems)
  (let (retval
        (index 0))
    (dolist (pollitem pollitems
             (nreverse retval))
      (destructuring-bind ((&key socket fd event) body) pollitem
        (push `(setf (ff:fslot-value %items ,index 'socket) ,(or socket 0)
                     (ff:fslot-value %items ,index 'fd) ,(or fd -1)
                     (ff:fslot-value %items ,index 'events) ,(or event +pollin+)
                     (aref handlers ,index) ,body)
              retval)
        (incf index)))))

(defmacro do-polling (&rest clauses)
  (destructuring-bind
        (pollitems timeout always)
      (parse-poll-clauses clauses)
    (let ((nitems (length pollitems)))
    `(let ((handlers (make-array (list ,nitems))))
       (ff:with-static-fobject (%items `(:array %pollitem ,,nitems))
         ,@(init-pollitems pollitems)
         (loop
            (let ((count (%poll %items ,nitems ,(if timeout (first timeout) -1))))
              (if (plusp count)
                  (dotimes (i ,nitems)
                    (when (plusp (ff:fslot-value %items i 'revents))
                      (funcall (aref handlers i))
                      (when (zerop (decf count))
                        (return))))
                  ,@(when timeout
                      `((funcall ,(second timeout)))))
              ,@(when always
                  `((funcall ,always))))))))))

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
