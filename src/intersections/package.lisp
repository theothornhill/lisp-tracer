(defpackage #:lisp-tracer-intersections
  (:use #:cl
        #:lisp-tracer-utilities
        #:lisp-tracer-rays
        #:lisp-tracer-tuples
        #:lisp-tracer-matrices
        #:lisp-tracer-spheres)
  (:export #:intersect
           #:intersection
           #:tt
           #:object
           #:make-intersection
           #:intersections
           #:hit))
