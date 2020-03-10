(asdf:defsystem #:lisp-tracer-tests
  :description "Tests for ray tracer"
  :author "Theodor Thornhill <theothornhill@pm.me>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:rove
               #:iterate
               #:arrows
               #:lisp-tracer)
  :components ((:module "src/tests"
                :components ((:file "package")
                             (:file "tuple-tests")
                             (:file "matrix-tests")
                             (:file "ray-tests")
                             (:file "color-tests")
                             (:file "canvas-tests")
                             (:file "tracer-tests")
                             (:file "lights-tests")
                             (:file "materials-tests")
                             (:file "intersection-tests")
                             (:file "sphere-tests")))))
