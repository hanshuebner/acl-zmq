;; Copyright (c) 2009, 2010 Vitaly Mayatskikh <v.mayatskih@gmail.com>
;;
;; This file is part of CL-ZMQ.
;;
;; Vitaly Mayatskikh grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(defpackage #:zeromq
  (:nicknames :zmq)
  (:use :cl)
  (:export #:with-socket
           #:with-context
           #:do-polling

           #:msg
           #:msg-size
           #:msg-init-size
           #:msg-data-as-array
           #:msg-data-as-string
           #:msg-close

           #:bind
           #:connect
           #:send
           #:recv
           #:setsockopt

           #:error-again))