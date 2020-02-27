(defpackage #:lisp-tracer-intersections
  (:use #:cl
        #:lisp-tracer-utilities
        #:lisp-tracer-rays
        #:lisp-tracer-tuples)
  (:import-from #:lisp-tracer-spheres
                #:sphere)
  (:export #:intersect
           #:intersection
           #:tt
           #:object
           #:make-intersection
           #:intersections
           #:hit))
