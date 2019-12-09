(defpackage #:lisp-tracer-canvas
  (:use #:cl)
  (:import-from #:lisp-tracer-colors
                #:color!)
  (:export #:canvas!
           #:width
           #:height))
