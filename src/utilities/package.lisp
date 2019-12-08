(defpackage #:lisp-tracer-utilities
  (:use #:cl)
  (:import-from #:lisp-tracer-tuples
                #:tuple
                #:vec
                #:x
                #:y
                #:z
                #:w)
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
           #:->>))
