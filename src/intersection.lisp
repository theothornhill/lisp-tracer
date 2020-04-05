(in-package #:lisp-tracer)

(declaim (inline make-intersection))
(defun make-intersection (tt obj)
  "Creates an intersection with an OBJECT at a given time TT."
  (make-rt-intersection :tt tt :object obj))

(defun prepare-computations (i r)
  (declare (type rt-intersection i) (type ray r))
  (let* ((direction (ray-direction r))
         (comps-object (rt-intersection-object i))
         (comps-point (pos r (rt-intersection-tt i)))
         (comps-eyev (neg direction))
         (comps-normalv (normal-at comps-object comps-point))
         (inside? (< (dot comps-normalv comps-eyev) 0.0))
         (normalv (if inside? (neg comps-normalv) comps-normalv))
         (reflectv (reflect direction normalv))
         (comps-over-point (add comps-point
                                (mult normalv epsilon))))
    (make-computations
     :tt (rt-intersection-tt i)
     :object comps-object
     :inside? inside?
     :point comps-point
     :over-point comps-over-point
     :eyev comps-eyev
     :normalv normalv
     :reflectv reflectv)))

(declaim (inline tt<))
(defun tt< (a b)
  (< (rt-intersection-tt a) (rt-intersection-tt b)))

(defun intersections (&rest xs)
  "Sorted list of intersections. Compares by TT from smallest to largest."
  (if xs (sort xs #'tt<)))

(defun intersect (shape ray)
  (let ((local-ray (transform ray (inverse (shape-transform shape)))))
    (local-intersect shape local-ray)))

(defun hit (xs)
  "If HIT is found with TT > 0, return the HIT"
  (declare (type list xs))
  (cond ((null xs) nil)
        ((> (rt-intersection-tt (car xs)) 0) (car xs))
        (t (hit (cdr xs)))))

(defun intersect-world (world ray)
  (sort (mapcan #'(lambda (s) (intersect s ray))
                (world-objects world))
        #'tt<))
