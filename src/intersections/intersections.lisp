(in-package #:lisp-tracer-intersections)

(defclass ray-tracer-intersection ()
  ((tt
    :initarg :tt
    :accessor tt
    :documentation "Time of the intersection.")
   (object
    :initarg :object
    :accessor object
    :documentation "The intersected object."))
  (:documentation "Structure keeping track of the intersected object at a given time."))

(defun make-intersection (tt obj)
  "Creates an intersection with an OBJECT at a given time TT."
  (make-instance 'ray-tracer-intersection :tt tt :object obj))

(defun intersections (&rest xs)
  "Sorted list of intersections. Compares by TT from smallest to largest."
  (labels ((tt< (a b) (< (tt a) (tt b))))
    (if xs (sort xs #'tt<))))

(defun intersect (sphere ray)
  "Intersect SPHERE with RAY"
  (declare (sphere sphere) (ray ray))
  (let* ((ray2 (transform ray (inverse (transform-matrix sphere))))
         (sphere-to-ray (sub (origin ray2) (make-point 0 0 0)))
         (a (dot (direction ray2) (direction ray2)))
         (b (mult 2 (dot (direction ray2) sphere-to-ray)))
         (c (sub (dot sphere-to-ray sphere-to-ray) 1))
         (discriminant (sub (mult b b) (reduce 'mult (list 4 a c)))))
    (unless (< discriminant 0)
      (intersections (make-intersection (div (sub (- b) (sqrt discriminant))
                                             (mult 2 a))
                                        sphere)
                     (make-intersection (div (add (- b) (sqrt discriminant))
                                             (mult 2 a))
                                        sphere)))))

(defun hit (xs)
  "If HIT is found with TT > 0, return the HIT"
  (declare (type list xs))
  (cond ((null xs) nil)
        ((> (tt (car xs)) 0) (car xs))
        (t (hit (cdr xs)))))
