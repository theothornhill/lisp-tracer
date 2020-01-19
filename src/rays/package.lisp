(defpackage #:lisp-tracer-rays
  (:use #:cl)
  (:export #:ray
           #:ray!
           #:origin
           #:direction
           #:pos)
  (:import-from #:lisp-tracer-tuples
                #:tuple
                #:point!
                #:vec!)
  (:import-from #:lisp-tracer-utilities
                #:add
                #:mult))
