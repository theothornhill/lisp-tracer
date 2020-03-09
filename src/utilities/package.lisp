(defpackage #:lisp-tracer-utilities
  (:use #:cl
        #:iterate
        #:arrows
        #:lisp-tracer-tuples
        #:lisp-tracer-colors
        #:lisp-tracer-matrices)
  (:export #:add
           #:sub
           #:mult
           #:div
           #:neg
           #:equal?
           #:magnitude
           #:normalize
           #:dot
           #:reflect
           #:cross
           #:maptuple
           #:mapnumber
           #:mapcolor
           #:tuple-to-list
           #:transform-object))
