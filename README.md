# acl-zmq: Allegro Common Lisp interface to 0MQ

This library implements an interface to the 0MQ message queueing
system for Allegro Common Lisp (ACL).  It is a fork of the cl-zmq
library (http://repo.or.cz/w/cl-zmq.git) which is based on CFFI.  As
CFFI does not support the style of errno passing that ACL uses and
could not easily be fixed to do that, all CFFI code was replaced by
native ACL FFI calls.  Also, the API of acl-zmq has deviated from
cl-zmq so that it is more idiomatic (ugh) Common Lisp and supports a
consless mode for polling queues.

acl-zmq has been implemented and tested with Allegro CL 8.2, 0MQ
version 2 as of June 2010 from http://github.com/zeromq/zeromq2 on
Linux x86/64.  There may be integer length issues with 32 bit
platforms.

Like cl-zmq, acl-zmq is distributed under the LLGPL
(http://opensource.franz.com/preamble.html).s