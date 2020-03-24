;;;; lisp-tracer.asd
(setf *read-default-float-format* 'double-float)

(asdf:defsystem :lisp-tracer
  :description "Ray tracer"
  :author "Theodor Thornhill <theothornhill@pm.me>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:iterate #:sb-sprof)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "structs")
                             (:file "tuple")
                             (:file "matrix")
                             (:file "util")
                             (:file "material")
                             (:file "math")
                             (:file "ray")
                             (:file "sphere")
                             (:file "world")
                             (:file "intersection")
                             (:file "canvas")
                             (:file "light")
                             (:file "camera")))
               (:module "src/renders"
                :serial t
                :components ((:file "package")
                             (:file "circle")))))
