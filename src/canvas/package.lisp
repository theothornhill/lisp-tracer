(defpackage #:lisp-tracer-canvas
  (:use #:cl #:iterate #:lisp-tracer-colors)
  (:export #:make-canvas
           #:width
           #:height
           #:canvas
           #:canvas-to-ppm
           #:write-pixel
           #:pixel-at))
