;;;; lisp-tracer.asd
(setf *read-default-float-format* 'double-float)

(asdf:defsystem :lisp-tracer
  :description "Ray tracer"
  :author "Theodor Thornhill <theothornhill@pm.me>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (:sb-sprof)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "structs")
                             (:file "tuple")
                             (:file "matrix")
                             (:file "util")
                             (:file "material")
                             (:file "math")
                             (:file "ray")
                             (:file "world")
                             (:file "intersection")
                             (:file "canvas")
                             (:file "light")
                             (:file "camera")
                             (:file "pattern")))
               (:module "src/shapes"
                :serial t
                :components ((:file "shape")
                             (:file "sphere")
                             (:file "plane")
                             (:file "cube")
                             (:file "cylinder")
                             (:file "cone")
                             (:file "group")))
               (:module "src/renders"
                :serial t
                :components ((:file "package")
                             (:file "circle")))))
