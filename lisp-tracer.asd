;;;; lisp-tracer.asd

(asdf:defsystem :lisp-tracer
  :description "Describe lisp-tracer here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:module "src/tuples"
                        :serial t
                        :components ((:file "package")
                                     (:file "tuples")))
               (:module "src/utilities"
                        :serial t
                        :components ((:file "package")
                                     (:file "macros")
                                     (:file "math")))
               (:module "src/tracer"
                        :serial t
                        :components ((:file "package")
                                     (:file "chapter01")))))
