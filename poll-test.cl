(defpackage :zmq-poll-test
  (:use :cl))

(in-package :zmq-poll-test)

(defun server (&key (address "tcp://lo:8018"))
  (zmq:with-context (ctx 1)
    (zmq:with-socket (s ctx zmq:+rep+)
      (zmq:bind s address)
      (let ((msg (make-instance 'zmq:msg)))
        (zmq:do-polling
          ((:socket s zmq:+pollin+)
           (zmq:recv s msg)
           (zmq:send s msg))
          ((:timeout 10000)
           ))))))

(defun client (&key (address "tcp://127.0.0.1:8018") (count 1000) (size 65536))
  (zmq::with-context (ctx 1)
    (zmq:with-socket (s ctx zmq:+req+)
      (zmq:connect s address)
      (let ((msg (make-instance 'zmq:msg :size size)))
        (dotimes (i count)
          (zmq:send s msg)
          (zmq:recv s msg))))))