(defpackage #:lisp-tracer-intersections
  (:use #:cl
        #:arrows
        #:lisp-tracer-tuples
        #:lisp-tracer-matrices
        #:lisp-tracer-utilities
        #:lisp-tracer-rays
        #:lisp-tracer-spheres)
  (:export #:intersect
           #:intersection
           #:tt
           #:object
           #:make-intersection
           #:intersections
           #:hit))
