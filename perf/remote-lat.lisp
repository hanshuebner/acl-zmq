(require :cl-zmq)

(defpackage :zmq-test
  (:use :cl :cffi))

(in-package :zmq-test)

(load "lat-parms")

(defvar *elapsed* nil)
(defvar *latency* nil)

(zmq::with-context (ctx 1 1)
  (zmq:with-socket (s ctx zmq:rep)
    (zmq:connect s *address*)
    (let ((msg (zmq:make-message *message-size*)))
      (setf *elapsed*
	    (zmq:with-stopwatch
		(dotimes (i *roundtrip-count*)
		  (zmq:send s msg 0)
		  (zmq:recv s msg 0)))))))

(setf *latency* (/ *elapsed* (* 2 *roundtrip-count*)))

(format t "message size: ~d [B]~%" *message-size*)
(format t "roundtrip count: ~d~%" *roundtrip-count*)
(format t "average latency: ~f [us]~%" *latency*)

(sb-ext:quit)
;
