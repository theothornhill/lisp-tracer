(defpackage #:lisp-tracer-spheres
  (:use #:cl
        #:arrows
        #:lisp-tracer-tuples
        #:lisp-tracer-utilities
        #:lisp-tracer-matrices
        #:lisp-tracer-rays)
  (:export #:sphere
           #:make-sphere
           #:transform-matrix
           #:set-transform
           #:normal-at))
