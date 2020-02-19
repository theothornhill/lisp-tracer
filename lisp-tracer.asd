;;;; lisp-tracer.asd

(asdf:defsystem :lisp-tracer
  :description "Ray tracer"
  :author "Theodor Thornhill <theothornhill@pm.me>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:module "src/tuples"
                        :serial t
                        :components ((:file "package")
                                     (:file "tuples")))
               (:module "src/matrices"
                        :serial t
                        :components ((:file "package")
                                     (:file "matrices")))
               (:module "src/colors"
                        :serial t
                        :components ((:file "package")
                                     (:file "colors")))
               (:module "src/utilities"
                        :serial t
                        :components ((:file "package")
                                     (:file "macros")
                                     (:file "math")))
               (:module "src/rays"
                        :serial t
                        :components ((:file "package")
                                     (:file "rays")))
               (:module "src/spheres"
                        :serial t
                        :components ((:file "package")
                                     (:file "spheres")))
               (:module "src/intersections"
                        :serial t
                        :components ((:file "package")
                                     (:file "intersections")))
               (:module "src/canvas"
                        :serial t
                        :components ((:file "package")
                                     (:file "canvas")))
               (:module "src/tracer"
                        :serial t
                        :components ((:file "package")
                                     (:file "clock")))))
