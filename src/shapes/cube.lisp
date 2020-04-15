(in-package #:lisp-tracer)

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

(defmethod local-intersect ((cube cube) (ray ray))
  (let ((x-origin (tuple-x (ray-origin ray)))
        (x-direction (tuple-x (ray-direction ray)))
        (y-origin (tuple-y (ray-origin ray)))
        (y-direction (tuple-y (ray-direction ray)))
        (z-origin (tuple-z (ray-origin ray)))
        (z-direction (tuple-z (ray-direction ray))))
    (multiple-value-bind (xtmin xtmax) (check-axis x-origin x-direction)
      (multiple-value-bind (ytmin ytmax) (check-axis y-origin y-direction)
        (multiple-value-bind (ztmin ztmax) (check-axis z-origin z-direction)
          (let ((tmin (max xtmin ytmin ztmin))
                (tmax (min xtmax ytmax ztmax)))
            (unless (> tmin tmax)
              (intersections
               (make-intersection :tt tmin :object cube)
               (make-intersection :tt tmax :object cube)))))))))

(defmethod local-normal-at ((cube cube) (point tuple))
  (let* ((abs-x (abs (tuple-x point)))
         (abs-y (abs (tuple-y point)))
         (abs-z (abs (tuple-z point)))
         (maxc (max abs-x abs-y abs-z)))
    (cond
      ((equal? maxc abs-x) (make-vec :x (tuple-x point) :y 0.0 :z 0.0))
      ((equal? maxc abs-y) (make-vec :x 0.0 :y (tuple-y point) :z 0.0))
      ((equal? maxc abs-z) (make-vec :x 0.0 :y 0.0 :z (tuple-z point))))))


