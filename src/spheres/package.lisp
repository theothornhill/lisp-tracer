(defpackage #:lisp-tracer-spheres
  (:use #:cl)
  (:import-from #:lisp-tracer-rays
                #:ray)
  (:export #:sphere
           #:sphere!
           #:intersect))
