(in-package #:lisp-tracer)

(declaim (inline make-intersection))
(defun make-intersection (tt obj)
  "Creates an intersection with an OBJECT at a given time TT."
  (make-rt-intersection :tt tt :object obj))

(defun prepare-computations (i r)
  (declare (type rt-intersection i) (type ray r))
  (let* ((comps-object (rt-intersection-object i))
         (comps-point (pos r (rt-intersection-tt i)))
         (comps-eyev (neg (ray-direction r)))
         (comps-normalv (normal-at comps-object comps-point))
         (inside? (< (dot comps-normalv comps-eyev) 0.0))
         (normalv (if inside? (neg comps-normalv) comps-normalv))
         (comps-over-point (add comps-point
                                (mult normalv epsilon))))
    (make-computations
     :tt (rt-intersection-tt i)
     :object comps-object
     :inside? inside?
     :point comps-point
     :over-point comps-over-point
     :eyev comps-eyev
     :normalv normalv)))

(declaim (inline tt<))
(defun tt< (a b)
  (< (rt-intersection-tt a) (rt-intersection-tt b)))

(defun intersections (&rest xs)
  "Sorted list of intersections. Compares by TT from smallest to largest."
  (if xs (sort xs #'tt<)))

(defun intersect (sphere ray)
  "Intersect SPHERE with RAY"
  (declare (sphere sphere) (ray ray))
  (let* ((ray2 (transform ray (inverse (sphere-transform sphere))))
         (sphere-to-ray (sub (ray-origin ray2) (make-point 0.0 0.0 0.0)))
         (ray2-direction (ray-direction ray2))
         (a (dot ray2-direction ray2-direction))
         (b (mult 2.0 (dot ray2-direction sphere-to-ray)))
         (c (sub (dot sphere-to-ray sphere-to-ray) 1.0))
         (discriminant (sub (mult b b)
                            (mult 4.0 (mult a c)))))
    (declare (type double-float discriminant a b c))
    (unless (< discriminant 0.0)
      (intersections (make-intersection (/ (- (- b) (sqrt discriminant))
                                           (* 2.0 a))
                                        sphere)
                     (make-intersection (/ (+ (- b) (sqrt discriminant))
                                           (* 2.0 a))
                                        sphere)))))

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
