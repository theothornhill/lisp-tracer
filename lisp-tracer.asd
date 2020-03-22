;;;; lisp-tracer.asd

(asdf:defsystem :lisp-tracer
  :description "Ray tracer"
  :author "Theodor Thornhill <theothornhill@pm.me>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:iterate)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "tuple")
                             (:file "matrix")
                             (:file "color")
                             (:file "util")
                             (:file "material")
                             (:file "math")
                             (:file "ray")
                             (:file "sphere")
                             (:file "intersection")
                             (:file "canvas")
                             (:file "light")
                             (:file "world")
                             (:file "camera")))
               (:module "src/renders"
                :serial t
                :components ((:file "package")
                             (:file "circle")))))
