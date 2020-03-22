(in-package #:lisp-tracer)

(defstruct camera
  (hsize 0.0 :type single-float)
  (vsize 0.0 :type single-float )
  (half-width 0.0 :type single-float)
  (half-height 0.0 :type single-float)
  (field-of-view 0.0 :type single-float)
  (transform (identity-matrix) :type matrix)
  (pixel-size 0.0 :type single-float))


(defun compute-pixel-size (camera)
  (let ((half-view (tan (div (camera-field-of-view camera) 2.0)))
        (aspect (div (camera-hsize camera) (camera-vsize camera))))
    (if (>= aspect 1.0)
        (progn
          (setf (camera-half-width camera) half-view)
          (setf (camera-half-height camera) (div half-view aspect)))
        (progn
          (setf (camera-half-width camera) (mult half-view aspect))
          (setf (camera-half-height camera) half-view)))
    (setf (camera-pixel-size camera)
          (div (mult (camera-half-width camera) 2.0)
               (camera-hsize camera)))))


(defun create-camera (hsize vsize field-of-view)
  (let ((c (make-camera :hsize hsize
                        :vsize vsize
                        :field-of-view field-of-view)))
    (compute-pixel-size c)
    c))

(defun ray-for-pixel (camera px py)
  (let* ((x-offset (mult (add px 0.5) (camera-pixel-size camera)))
         (y-offset (mult (add py 0.5) (camera-pixel-size camera)))
         (world-x (sub (camera-half-width camera) x-offset))
         (world-y (sub (camera-half-height camera) y-offset))
         (pixel (mult
                 (inverse (camera-transform camera))
                 (make-point world-x world-y -1.0)))
         (origin (mult
                  (inverse (camera-transform camera))
                  (make-point 0.0 0.0 0.0)))
         (direction (normalize (sub pixel origin))))
    (make-ray :origin origin :direction direction)))