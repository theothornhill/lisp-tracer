(defpackage #:lisp-tracer-utilities
  (:use #:cl)
  (:import-from #:lisp-tracer-tuples
                #:tuple
                #:make-tuple
                #:make-point
                #:make-vec
                #:x
                #:y
                #:z
                #:w)
  (:import-from #:lisp-tracer-colors
                #:color
                #:make-color
                #:red
                #:green
                #:blue)
  (:import-from #:lisp-tracer-matrices
                #:make-matrix
                #:matrix
                #:m
                #:grid
                #:dimensions)
  (:export #:add
           #:sub
           #:mult
           #:div
           #:neg
           #:equal?
           #:magnitude
           #:normalize
           #:dot
           #:cross
           #:->
           #:->>
           #:maptuple
           #:mapnumber
           #:mapcolor
           #:tuple-to-list
           #:transform-object))
