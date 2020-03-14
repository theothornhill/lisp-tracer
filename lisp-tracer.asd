;;;; lisp-tracer.asd

(asdf:defsystem :lisp-tracer
  :description "Ray tracer"
  :author "Theodor Thornhill <theothornhill@pm.me>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:iterate)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "tuples")
                             (:file "matrices")
                             (:file "colors")
                             (:file "utils")
                             (:file "materials")
                             (:file "math")
                             (:file "rays")
                             (:file "spheres")
                             (:file "intersections")
                             (:file "canvas")
                             (:file "lights")))
               (:module "src/renders"
                :serial t
                :components ((:file "package")
                             (:file "clock")
                             (:file "circle")))))
