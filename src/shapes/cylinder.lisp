(in-package #:lisp-tracer)

(defstruct (cylinder (:include shape))
  (closed nil :type t)
  (minimum sb-ext:double-float-negative-infinity :type double-float)
  (maximum sb-ext:double-float-positive-infinity :type double-float))

(declaim (inline check-cylinder-cap))
(defun check-cylinder-cap (ray tt)
  (with-ray-slots ray (((ori-x x) (ori-z z))
                       ((dir-x x) (dir-z z)))
    (multiple-value-bind (x z)
        (values (+ ori-x (* tt dir-x)) (+ ori-z (* tt dir-z)))
      (<= (+ (expt x 2) (expt z 2)) 1.0))))

(defmethod local-intersect ((cylinder cylinder) (ray ray))
  (append (intersect-cylinder cylinder ray)
          (intersect-cylinder-caps cylinder ray)))

(defun intersect-cylinder (cylinder ray)
  (with-all-ray-slots ray
    (let ((a (+ (expt dir-x 2) (expt dir-z 2))))
      (unless (equal? a 0.0)
        (let* ((b (+ (* 2.0 ori-x dir-x) (* 2.0 ori-z dir-z)))
               (c (- (+ (expt ori-x 2) (expt ori-z 2)) 1.0))
               (discriminant (- (expt b 2) (* 4.0 a c))))
          (unless (< discriminant 0)
            (let* ((-b (- b))
                   (sqrt-discriminant (sqrt discriminant))
                   (2a (* 2.0 a))
                   (t0 (/ (- -b sqrt-discriminant) 2a))
                   (t1 (/ (+ -b sqrt-discriminant) 2a)))
              (if (> t0 t1) (rotatef t0 t1))
              (with-slots (minimum maximum) cylinder
                (let* ((y0 (+ ori-y (* t0 dir-y)))
                       (y1 (+ ori-y (* t1 dir-y)))
                       (first-intersection
                         (if (< minimum y0 maximum)
                             (list (make-intersection :tt t0 :object cylinder))))
                       (second-intersection
                         (if (< minimum y1 maximum)
                             (list (make-intersection :tt t1 :object cylinder)))))
                  (append first-intersection second-intersection))))))))))

(defun intersect-cylinder-caps (cylinder ray)
  (with-ray-slots ray (((ori-y y))
                       ((dir-y y)))
    (with-slots (minimum maximum closed) cylinder
      (unless (or (not closed) (equal? dir-y 0.0))
        (let* ((t0 (/ (- minimum ori-y) dir-y))
               (t1 (/ (- maximum ori-y) dir-y))
               (first-intersection
                 (if (check-cylinder-cap ray t0)
                     (list (make-intersection :tt t0 :object cylinder))))
               (second-intersection
                 (if (check-cylinder-cap ray t1)
                     (list (make-intersection :tt t1 :object cylinder)))))
          (append first-intersection second-intersection))))))

(defmethod local-normal-at ((cylinder cylinder) (point tuple))
  (with-slots (x y z) point
    (with-slots (minimum maximum) cylinder
      (cond-let ((distance (+ (expt x 2) (expt z 2))))
        ((and (< distance 1.0) (>= y (- maximum epsilon)))
         (make-vec :x 0.0 :y 1.0 :z 0.0))
        ((and (< distance 1.0) (<= y (+ minimum epsilon)))
         (make-vec :x 0.0 :y -1.0 :z 0.0))
        (t (make-vec :x x :y 0.0 :z z))))))
