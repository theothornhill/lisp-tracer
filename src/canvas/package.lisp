(defpackage #:lisp-tracer-canvas
  (:use #:cl)
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
           #:canv
           #:canvas-to-ppm
           #:write-pixel
           #:pixel-at))
