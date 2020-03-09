;;;; lisp-tracer.asd

(asdf:defsystem :lisp-tracer
  :description "Ray tracer"
  :author "Theodor Thornhill <theothornhill@pm.me>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:iterate
               #:arrows)
  :components ((:module "src/tuples"
                :components ((:file "package")
                             (:file "tuples")))
               (:module "src/matrices"
                :components ((:file "package")
                             (:file "matrices")))
               (:module "src/colors"
                :components ((:file "package")
                             (:file "colors")))
               (:module "src/utilities"
                :components ((:file "package")
                             (:file "utils")
                             (:file "math")))
               (:module "src/rays"
                :components ((:file "package")
                             (:file "rays")))
               (:module "src/spheres"
                :components ((:file "package")
                             (:file "spheres")))
               (:module "src/intersections"
                :components ((:file "package")
                             (:file "intersections")))
               (:module "src/canvas"
                :components ((:file "package")
                             (:file "canvas")))
               (:module "src/lights"
                :components ((:file "package")
                             (:file "lights")))
               (:module "src/tracer"
                :components ((:file "package")
                             (:file "clock")))))
