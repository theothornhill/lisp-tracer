(defpackage #:lisp-tracer-canvas
  (:use #:cl)
  (:import-from #:lisp-tracer-colors
                #:black
                #:color
                #:color!
                #:red
                #:green
                #:blue)
  (:export #:canvas!
           #:width
           #:height
           #:canv
           #:canvas-to-ppm!
           #:write-pixel!
           #:pixel-at))
