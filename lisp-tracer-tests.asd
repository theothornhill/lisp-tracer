(setf *read-default-float-format* 'double-float)

(asdf:defsystem #:lisp-tracer-tests
  :description "Tests for ray tracer"
  :author "Theodor Thornhill <theothornhill@pm.me>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:rove
               #:lisp-tracer)
  :components ((:module "src/tests"
                :components ((:file "package")
                             (:file "tuple-tests")
                             (:file "matrix-tests")
                             (:file "ray-tests")
                             (:file "color-tests")
                             (:file "canvas-tests")
                             (:file "tracer-tests")
                             (:file "light-tests")
                             (:file "material-tests")
                             (:file "intersection-tests")
                             (:file "shape-tests")
                             (:file "plane-tests")
                             (:file "world-tests")
                             (:file "camera-tests")
                             (:file "pattern-tests")))))
