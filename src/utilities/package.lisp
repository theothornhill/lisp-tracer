(defpackage #:lisp-tracer-utilities
  (:use #:cl)
  (:import-from #:lisp-tracer-tuples
                #:tuple
                #:tuple!
                #:point!
                #:vec!
                #:x
                #:y
                #:z
                #:w)
  (:import-from #:lisp-tracer-colors
                #:color
                #:color!
                #:red
                #:green
                #:blue)
  (:import-from #:lisp-tracer-matrices
                #:matrix!
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
           #:tuple-to-list))
