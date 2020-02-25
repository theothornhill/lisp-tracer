(asdf:defsystem #:lisp-tracer-tests
  :description "Describe lisp-tracer here"
  :author "Theodor Thornhill <theothornhill@pm.me>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:rove
               #:lisp-tracer)
  :components ((:module "src/tests"
                        :serial t
                        :components ((:file "package")
                                     (:file "tuple-tests")
                                     (:file "matrix-tests")
                                     (:file "ray-tests")
                                     (:file "color-tests")
                                     (:file "canvas-tests")
                                     (:file "tracer-tests")
                                     (:file "intersection-tests")
                                     (:file "sphere-tests")))))
