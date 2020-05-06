(in-package #:lisp-tracer)

(defstruct (cone (:include shape))
  (closed nil :type t)
  (minimum sb-ext:double-float-negative-infinity :type double-float)
  (maximum sb-ext:double-float-positive-infinity :type double-float))

(declaim (inline check-cone-cap))
(defun check-cone-cap (ray tt r)
  (with-slots (origin direction) ray
    (let ((x (+ (tuple-x origin) (* tt (tuple-x direction))))
          (z (+ (tuple-z origin) (* tt (tuple-z direction)))))
      (<= (+ (expt x 2) (expt z 2)) (expt r 2)))))

(defmethod local-intersect ((cone cone) (ray ray))
  (append (intersect-cone cone ray)
          (intersect-cone-caps cone ray)))

(defun intersect-cone (cone ray)
  (with-ray-slots ray (((o-x x) (o-y y) (o-z z))
                       ((d-x x) (d-y y) (d-z z)))
    (let ((a (+ (- (expt d-x 2) (expt d-y 2)) (expt d-z 2)))
          (b (+ (- (* 2.0 o-x d-x) (* 2.0 o-y d-y))
                (* 2.0 o-z d-z)))
          (c (+ (- (expt o-x 2) (expt o-y 2)) (expt o-z 2))))
      (unless (and (equal? a 0.0) (equal? b 0.0))
        (if (equal? a 0.0)
            (list (make-intersection :tt (/ (- c) (* 2.0 b)) :object cone))
            (let* ((discriminant (- (expt b 2) (* 4.0 a c))))
              (unless (< discriminant 0)
                (let* ((-b (- b))
                       (sqrt-discriminant (sqrt discriminant))
                       (2a (* 2.0 a))
                       (t0 (/ (- -b sqrt-discriminant) 2a))
                       (t1 (/ (+ -b sqrt-discriminant) 2a)))
                  (if (> t0 t1) (rotatef t0 t1))
                  (with-slots (minimum maximum) cone
                    (let* ((y0 (+ o-y (* t0 d-y)))
                           (y1 (+ o-y (* t0 d-y)))
                           (first-intersection
                             (if (< minimum y0 maximum)
                                 (list (make-intersection :tt t0 :object cone))))
                           (second-intersection
                             (if (< minimum y1 maximum)
                                 (list (make-intersection :tt t1 :object cone)))))
                      (append first-intersection second-intersection)))))))))))

(defun intersect-cone-caps (cone ray)
  (with-ray-slots ray (((o-y y)) ((d-y y)))
    (unless (or (not (cone-closed cone)) (equal? d-y 0.0))
      (labels ((t-value (min-or-max) (/ (- min-or-max o-y) d-y)))
        (with-slots (minimum maximum) cone
          (let* ((t1 (t-value minimum))
                 (t2 (t-value maximum))
                 (first-intersection
                   (when (check-cone-cap ray t1 minimum)
                     (list (make-intersection :tt t1 :object cone))))
                 (second-intersection
                   (when (check-cone-cap ray t2 maximum)
                     (list (make-intersection :tt t2 :object cone)))))
            (append first-intersection second-intersection)))))))

  
(defmethod local-normal-at ((cone cone) (point tuple))
  (with-slots (x y z) point
    (let ((distance (+ (expt x 2) (expt z 2))))
      (with-slots (minimum maximum) cone
        (cond ((and (< distance 1.0) (>= y (- maximum epsilon)))
               (make-vec :x 0.0 :y 1.0 :z 0.0))
              ((and (< distance 1.0) (<= y (+ minimum epsilon)))
               (make-vec :x 0.0 :y -1.0 :z 0.0))
              (t (let* ((y-distance (sqrt distance)))
                   (make-vec :x x
                             :y (if (> y 0) (- y-distance) y-distance)
                             :z z))))))))
