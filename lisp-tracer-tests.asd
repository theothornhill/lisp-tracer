(asdf:defsystem #:lisp-tracer-tests
  :description "Describe lisp-tracer here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:rove
               #:lisp-tracer)
  :components ((:module "src/tests"
                        :serial t
                        :components ((:file "package")
                                     (:file "tuple-tests")
                                     (:file "color-tests")
                                     (:file "canvas-tests")
                                     (:file "tracer-tests")))))
