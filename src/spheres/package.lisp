(defpackage #:lisp-tracer-spheres
  (:use #:cl
        #:lisp-tracer-utilities
        #:lisp-tracer-rays)
  (:import-from #:lisp-tracer-tuples
                #:make-point)
  (:export #:sphere
           #:make-sphere
           #:intersect))
