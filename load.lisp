(defun ld (file)
  (load (compile-file file)))

(ld "package")
(ld "meta-acl")
(ld "zeromq-ffi")
(ld "zeromq-api")
(ld "perf-tests")