(defpackage #:lisp-tracer-rays
  (:use #:cl)
  (:export #:ray
           #:make-ray
           #:origin
           #:direction
           #:transform
           #:pos)
  (:import-from #:lisp-tracer-matrices
                #:matrix)
  (:import-from #:lisp-tracer-tuples
                #:tuple
                #:make-point
                #:make-vec)
  (:import-from #:lisp-tracer-utilities
                #:add
                #:mult
                #:transform-object))
