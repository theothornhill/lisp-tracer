(defpackage #:lisp-tracer-tuples
  (:use #:cl
        #:lisp-tracer-utilities)
  (:export #:make-tuple
           #:tuple
           #:point
           #:make-point
           #:vec
           #:make-vec
           #:reflect
           #:x
           #:y
           #:z
           #:w
           #:point?
           #:vec?
           #:epsilon
           #:zerovec
           #:to-pixel))
