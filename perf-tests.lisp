;; Copyright (c) 2009, 2010 Vitaly Mayatskikh <v.mayatskih@gmail.com>
;;
;; This file is part of CL-ZMQ.
;;
;; Vitaly Mayatskikh grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; The performance tests are basically a translation of the 0MQ
;; performance tests written in C, and they can be run against their C
;; counterparts.

(defpackage :zeromq-test
  (:use :cl))

(in-package :zeromq-test)

;; common defaults

(defparameter *message-count* 1000)
(defparameter *message-size* 32)

;; throughput test defaults

(defparameter *bind-address* "tcp://lo:8000")
(defparameter *connect-address* "tcp://127.0.0.1:8000")
(defparameter *rate* 256)

;; latency test defaults

(defparameter *address* "tcp://127.0.0.1:5555")
(defparameter *roundtrip-count* 1000)

(defmacro with-stopwatch (&body body)
  (let ((sec0 (gensym))
	(sec1 (gensym))
	(usec0 (gensym))
	(usec1 (gensym)))
    `(multiple-value-bind (,sec0 ,usec0)
	 (excl::acl-internal-real-time)
       (unwind-protect
	    (progn ,@body))
       (multiple-value-bind (,sec1 ,usec1)
	   (excl::acl-internal-real-time)
	 (+ (* 1e6 (- ,sec1 ,sec0))
	    ,usec1 (- ,usec0))))))

(defun local-lat (&key (address *address*)
                       (roundtrip-count *roundtrip-count*)
                       (message-size *message-size*)
                       non-blocking)
  (declare (ignore message-size))
  (zmq:with-context (ctx 1)
    (zmq:with-socket (s ctx :rep)
      (zmq:bind s address)
      (let ((msg (make-instance 'zmq:msg)))
        (flet ((do-one-roundtrip ()
                 (if non-blocking
                     (tagbody retry
                        (handler-case
                            (progn
                              (zmq:recv s msg :noblock)
                              (format t "size ~d, ~a~%" (zmq:msg-size msg) (zmq:msg-data-as-array msg)))
                          (zmq:error-again (c)
                            (declare (ignore c))
                            (sleep 0.01)
                            (go retry))))
                     (progn
                       (zmq:recv s msg)
                       (zmq:send s msg)))))
          (if roundtrip-count
              (dotimes (i roundtrip-count)
                (do-one-roundtrip))
              (loop
                 (do-one-roundtrip))))))))

(defun remote-lat (&key (address *address*)
                        (roundtrip-count *roundtrip-count*)
                        (message-size *message-size*))
  (let (elapsed)
    (zmq::with-context (ctx 1)
      (zmq:with-socket (s ctx :req)
        (zmq:connect s address)
        (let ((msg (make-instance 'zmq:msg :size message-size)))
          (setf elapsed
                (with-stopwatch
                  (dotimes (i roundtrip-count)
                    (zmq:send s msg)
                    (zmq:recv s msg)))))))

    (let ((latency (/ elapsed (* 2 roundtrip-count))))
      (format t "message size: ~d [B]~%" message-size)
      (format t "roundtrip count: ~d~%" roundtrip-count)
      (format t "average latency: ~f [us]~%" latency))))

(defun local-thr (&key (bind-address *bind-address*)
                       (rate *rate*)
                       (message-size *message-size*)
                       (message-count *message-count*))
  (let (elapsed)
    (zmq::with-context (ctx 1)
      (zmq:with-socket (s ctx :sub)
        (zmq:setsockopt s :subscribe "")
        (zmq:setsockopt s :rate rate)
        (zmq:bind s bind-address)
        (let ((msg (make-instance 'zmq:msg)))
          (zmq:recv s msg)
          (setf elapsed
                (with-stopwatch
                  (dotimes (i (1- message-count))
                    (zmq:recv s msg)))))))
    (let* ((throughput (* (/ message-count elapsed) 1e6))
           (megabits (/ (* throughput message-count 8) 1e6)))

      (format t "message size: ~d [B]~%" message-size)
      (format t "message count: ~d~%" message-count)
      (format t "mean throughput: ~d [msg/s]~%" (round throughput))
      (format t "mean throughput: ~,3f [Mb/s]~%" megabits))))

(defun remote-thr (&key (connect-address *connect-address*)
                        (rate *rate*)
                        (message-size *message-size*)
                        (message-count *message-count*))
  (zmq::with-context (ctx 1)
    (zmq:with-socket (s ctx :upstream)
      (zmq:setsockopt s :rate rate)
      (zmq:connect s connect-address)
      (let ((msg (make-instance 'zmq:msg)))
        (dotimes (i message-count)
          (zmq:msg-init-size msg message-size)
          (zmq:send s msg)
          (zmq:msg-close msg))))))
