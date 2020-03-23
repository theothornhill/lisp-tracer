(in-package #:lisp-tracer)

(defstruct rt-intersection
  (tt nil :type single-float)
  (object nil :type sphere))

(defstruct computations
  (tt 0.0 :type single-float)
  (object nil :type sphere)
  (inside? nil :type t)
  (point (make-point 0.0 0.0 0.0) :type tuple)
  (over-point (make-point 0.0 0.0 0.0) :type tuple)
  (eyev (make-vec 0.0 0.0 0.0) :type tuple)
  (normalv (make-vec 0.0 0.0 0.0) :type tuple))

(declaim (inline make-intersection))
(defun make-intersection (tt obj)
  "Creates an intersection with an OBJECT at a given time TT."
  (declare (optimize (speed 3) (safety 0)))
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
                                ;; This 0.003 value is completely
                                ;; arbitrary and should be changed.
                                ;; I need to find a better way to
                                ;; avoid acne...
                                (mult normalv 0.003))))
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
  (declare (optimize (speed 3) (safety 0)))
  (< (rt-intersection-tt a) (rt-intersection-tt b)))

(defun intersections (&rest xs)
  "Sorted list of intersections. Compares by TT from smallest to largest."
  (declare (optimize (speed 3) (safety 0)))
  (if xs (sort xs #'tt<)))


;; TODO: This is an attempt to memoize the actual transform matrix
;; so that we don't have to run inverse all the time. This optimization
;; yields quite a bit of a performance increase, but I'm still unsure
;; whether it is safe to only store one matrix. What if we have more
;; spheres etc.
;; (defvar inverted-matrix nil)
;; (declaim (inline store-inverted-matrix))
;; (defun store-inverted-matrix (sphere)
;;   (declare (optimize (speed 3) (safety 0)))
;;   (if inverted-matrix
;;       inverted-matrix
;;       (let ((inv (inverse (sphere-transform sphere))))
;;         (setf inverted-matrix inv)
;;         inv)))

(defun intersect (sphere ray)
  "Intersect SPHERE with RAY"
  (declare (sphere sphere) (ray ray)
           (optimize (speed 3) (safety 0)))
  (let* ((ray2 (transform ray (inverse (sphere-transform sphere))))
         (sphere-to-ray (sub (ray-origin ray2) (make-point 0.0 0.0 0.0)))
         (ray2-direction (ray-direction ray2))
         (a (dot ray2-direction ray2-direction))
         (b (mult 2.0 (dot ray2-direction sphere-to-ray)))
         (c (sub (dot sphere-to-ray sphere-to-ray) 1.0))
         (discriminant (sub (mult b b)
                            (mult 4.0 (mult a c)))))
    (declare (type single-float discriminant a b c))
    (unless (< discriminant 0f0)
      (intersections (make-intersection (div (sub (- b) (sqrt discriminant))
                                             (mult 2.0 a))
                                        sphere)
                     (make-intersection (div (add (- b) (sqrt discriminant))
                                             (mult 2.0 a))
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
