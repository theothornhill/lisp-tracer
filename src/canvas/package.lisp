(defpackage #:lisp-tracer-canvas
  (:use #:cl #:iterate)
  (:import-from #:lisp-tracer-colors
                #:black
                #:color
                #:make-color
                #:red
                #:green
                #:blue)
  (:export #:make-canvas
           #:width
           #:height
           #:canvas
           #:canvas-to-ppm
           #:write-pixel
           #:pixel-at))
