(in-package #:lisp-tracer-spheres)

(defclass sphere ()
  ((id
    :initarg :id
    :initform (gensym)
    :accessor id)))

(defun make-sphere ()
  (make-instance 'sphere))

(defclass intersection ()
  ((tt
    :initarg :tt
    :accessor tt)
   (object
    :initarg :object
    :accessor object)))

(defun make-intersection (tt sphere)
  (make-instance 'intersection :tt tt :object sphere))

(defun intersections (&rest xs)
  (if xs
      xs
      nil))

(defun intersect (sphere ray)
  (declare (sphere sphere) (ray ray))
  (let* ((sphere-to-ray (sub (origin ray) (make-point 0 0 0)))
         (a (dot (direction ray) (direction ray)))
         (b (mult 2 (dot (direction ray) sphere-to-ray)))
         (c (sub (dot sphere-to-ray sphere-to-ray) 1))
         (discriminant (sub (mult b b) (reduce 'mult (list 4 a c)))))
    (if (< discriminant 0)
        (intersections)
        (intersections (make-intersection (div (sub (- b) (sqrt discriminant))
                                               (mult 2 a))
                                          sphere)
                       (make-intersection (div (add (- b) (sqrt discriminant))
                                               (mult 2 a))
                                          sphere)))))
