(in-package #:lisp-tracer-intersections)

(defclass ray-tracer-intersection ()
  ((tt
    :initarg :tt
    :accessor tt)
   (object
    :initarg :object
    :accessor object)))

(defun make-intersection (tt obj)
  (make-instance 'ray-tracer-intersection :tt tt :object obj))

(defun intersections (&rest xs)
  (labels ((tt< (a b) (< (tt a) (tt b))))
    (if xs (sort xs #'tt<))))

(defun intersect (sphere ray)
  (declare (sphere sphere) (ray ray))
  (let* ((sphere-to-ray (sub (origin ray) (make-point 0 0 0)))
         (a (dot (direction ray) (direction ray)))
         (b (mult 2 (dot (direction ray) sphere-to-ray)))
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
  (declare (type list xs))
  (cond ((null xs) nil)
        ((> (tt (car xs)) 0) (car xs))
        (t (hit (cdr xs)))))
