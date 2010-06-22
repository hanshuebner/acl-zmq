(defpackage :zmq-poll-test
  (:use :cl))

(in-package :zmq-poll-test)

(defun server (&key (address "tcp://lo:8018"))
  (zmq:with-context (ctx 1)
    (zmq:with-socket (s ctx :rep)
      (zmq:bind s address)
      (format t "waiting for data, press <return> to exit~%")
      (let ((msg (make-instance 'zmq:msg)))
        (zmq:do-polling (:label polling)
          ((:socket s)
           (zmq:recv s msg)
           (zmq:send s msg))
          ((:fd 0)
           (format t "exiting~%")
           (return-from polling))
          ((:timeout 10000)
           ;; just so that acl threads can be scheduled
           ))))))

(defun client (&key (address "tcp://127.0.0.1:8018") (count 1000) (size 65536))
  (zmq::with-context (ctx 1)
    (zmq:with-socket (s ctx :req)
      (zmq:connect s address)
      (let ((msg (make-instance 'zmq:msg :size size)))
        (zmq:send s msg)
        (zmq:do-polling (:label polling)
          ((:socket s)
           (zmq:recv s msg)
           (if (plusp (decf count))
               (zmq:send s msg)
               (return-from polling)))
          ((:timeout 10000)
           ;; just so that acl threads can be scheduled
           ))))))