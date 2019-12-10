;;;; lisp-tracer.asd

(asdf:defsystem :lisp-tracer
  :description "Ray tracer in common lisp based on The Ray Tracer Challenge"
  :author "Theodor Thornhill <theothornhill@pm.me>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:module "src/tuples"
                        :serial t
                        :components ((:file "package")
                                     (:file "tuples")))
               (:module "src/colors"
                        :serial t
                        :components ((:file "package")
                                     (:file "colors")))
               (:module "src/utilities"
                        :serial t
                        :components ((:file "package")
                                     (:file "macros")
                                     (:file "math")))
               (:module "src/canvas"
                        :serial t
                        :components ((:file "package")
                                     (:file "canvas")))
               (:module "src/tracer"
                        :serial t
                        :components ((:file "package")
                                     (:file "clock")))))
