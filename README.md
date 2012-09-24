# acl-zmq: Allegro Common Lisp interface to 0MQ

This library implements an interface to the 0MQ message queueing
system for Allegro Common Lisp (ACL).  It is a fork of the
[cl-zmq](http://repo.or.cz/w/cl-zmq.git) library which is based on
[CFFI](http://common-lisp.net/project/cffi/).  As CFFI does not
support the style of errno passing that ACL uses and could not easily
be fixed to do that, all CFFI code was replaced by native ACL FFI
calls.  Also, the API of acl-zmq has deviated from cl-zmq so that it
is more idiomatic (ugh) Common Lisp and supports a consless mode for
polling queues.

acl-zmq has been implemented and tested with Allegro CL 8.2, 0MQ
version 2 as of June 2010 from
[github](http://github.com/zeromq/zeromq2) on Linux x86/64.  There may
be integer length issues with 32 bit platforms.

Like cl-zmq, acl-zmq is distributed under the
[LLGPL](http://opensource.franz.com/preamble.html).

The code has not been tested or used a lot, so it is likely to 
contain bugs.  As I am not currently using ACL and ZMQ, I cannot give
any support for it.  Please keep this in mind if you use it as a
starting point for your project.