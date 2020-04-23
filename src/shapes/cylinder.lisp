(in-package #:lisp-tracer)

(defstruct (cylinder (:include shape))
  (closed nil :type t)
  (minimum sb-ext:double-float-negative-infinity :type double-float)
  (maximum sb-ext:double-float-positive-infinity :type double-float))

(declaim (inline check-cylinder-cap))
(defun check-cylinder-cap (ray tt)
  (let ((x (+ (ray-origin-x ray) (* tt (ray-direction-x ray))))
        (z (+ (ray-origin-z ray) (* tt (ray-direction-z ray)))))
    (<= (+ (expt x 2) (expt z 2)) 1.0)))

(defmethod local-intersect ((cylinder cylinder) (ray ray))
  (append (intersect-cylinder cylinder ray)
          (intersect-cylinder-caps cylinder ray)))

(defun intersect-cylinder (cylinder ray)
  (let* ((a (+ (expt (ray-direction-x ray) 2)
               (expt (ray-direction-z ray) 2))))
    (unless (equal? a 0.0)
      (let* ((b (+ (* 2.0 (ray-origin-x ray) (ray-direction-x ray))
                   (* 2.0 (ray-origin-z ray) (ray-direction-z ray))))
             (c (- (+ (expt (ray-origin-x ray) 2) (expt (ray-origin-z ray) 2)) 1.0))
             (discriminant (- (expt b 2) (* 4.0 a c))))
        (unless (< discriminant 0)
          (let* ((-b (- b))
                 (sqrt-discriminant (sqrt discriminant))
                 (2a (* 2.0 a))
                 (t0 (/ (- -b sqrt-discriminant) 2a))
                 (t1 (/ (+ -b sqrt-discriminant) 2a)))
            (if (> t0 t1) (rotatef t0 t1))
            (let* ((y0 (+ (ray-origin-y ray) (* t0 (ray-direction-y ray))))
                   (y1 (+ (ray-origin-y ray) (* t1 (ray-direction-y ray))))
                   (first-intersection
                     (if (< (cylinder-minimum cylinder) y0 (cylinder-maximum cylinder))
                         (list (make-intersection :tt t0 :object cylinder))))
                   (second-intersection
                     (if (< (cylinder-minimum cylinder) y1 (cylinder-maximum cylinder))
                         (list (make-intersection :tt t1 :object cylinder)))))
              (append first-intersection second-intersection))))))))

(defun intersect-cylinder-caps (cylinder ray)
  (unless (or (not (cylinder-closed cylinder)) (equal? (ray-direction-y ray) 0.0))
    (let* ((t0 (/ (- (cylinder-minimum cylinder) (ray-origin-y ray))
                  (ray-direction-y ray)))
           (t1 (/ (- (cylinder-maximum cylinder) (ray-origin-y ray))
                  (ray-direction-y ray)))
           (first-intersection
             (if (check-cylinder-cap ray t0)
                 (list (make-intersection :tt t0 :object cylinder))))
           (second-intersection
             (if (check-cylinder-cap ray t1)
                 (list (make-intersection :tt t1 :object cylinder)))))
      (append first-intersection second-intersection))))

(defmethod local-normal-at ((cylinder cylinder) (point tuple))
  (let ((distance (+ (expt (tuple-x point) 2) (expt (tuple-z point) 2))))
    (cond ((and (< distance 1.0)
                (>= (tuple-y point) (- (cylinder-maximum cylinder) epsilon)))
           (make-vec :x 0.0 :y 1.0 :z 0.0))
          ((and (< distance 1.0)
                (<= (tuple-y point) (+ (cylinder-minimum cylinder) epsilon)))
           (make-vec :x 0.0 :y -1.0 :z 0.0))
          (t (make-vec :x (tuple-x point) :y 0.0 :z (tuple-z point))))))
