(in-package :lisp-tracer)

(defstruct (cube (:include shape)))

(defun check-axis
    (origin direction
     &aux
       (tmin-numerator (- -1.0 origin))
       (tmax-numerator (- 1.0 origin))
       (infinity sb-ext:double-float-positive-infinity)
       tmin
       tmax)
  (if (>= (abs direction) epsilon)
      (progn
        (setf tmin (/ tmin-numerator direction))
        (setf tmax (/ tmax-numerator direction)))
      (progn
        (setf tmin (* tmin-numerator infinity))
        (setf tmax (* tmax-numerator infinity))))
  (if (> tmin tmax) (values tmax tmin) (values tmin tmax)))

(defmacro multiple-bind-check ((&rest bindings) &body body)
  `(multiple-value-bind ,@(car bindings)
       (multiple-value-bind ,@(cadr bindings)
           (multiple-value-bind ,@(caddr bindings)
               ,@body))))

(defmethod local-intersect ((cube cube) (ray ray))
  (with-ray-slots ray (((ori-x x) (ori-y y) (ori-z z))
                       ((dir-x x) (dir-y y) (dir-z z)))
    (multiple-bind-check (((xtmin xtmax) (check-axis ori-x dir-x))
                          ((ytmin ytmax) (check-axis ori-y dir-y))
                          ((ztmin ztmax) (check-axis ori-z dir-z)))
      (multiple-value-bind (tmin tmax) (values (max xtmin ytmin ztmin)
                                               (min xtmax ytmax ztmax))
        (unless (> tmin tmax)
          (intersections
           (make-intersection :tt tmin :object cube)
           (make-intersection :tt tmax :object cube)))))))

(defmethod local-normal-at ((cube cube) (point tuple))
  (with-slots (x y z) point
    (multiple-value-bind (abs-x abs-y abs-z) (values (abs x) (abs y) (abs z))
      (cond-let ((maxc (max abs-x abs-y abs-z)))
        ((equal? maxc abs-x) (make-vec :x x :y 0.0 :z 0.0))
        ((equal? maxc abs-y) (make-vec :x 0.0 :y y :z 0.0))
        ((equal? maxc abs-z) (make-vec :x 0.0 :y 0.0 :z z))))))


