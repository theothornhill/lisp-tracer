(in-package #:lisp-tracer)

(defstruct camera
  (hsize 0 :type integer)
  (vsize 0 :type integer)
  (half-width 0.0 :type single-float)
  (half-height 0.0 :type single-float)
  (field-of-view 0.0 :type single-float)
  (transform (identity-matrix) :type matrix)
  (pixel-size 0.0 :type single-float))


(defun compute-pixel-size (camera)
  (let ((half-view (tan (float (/ (camera-field-of-view camera) 2.0))))
        (aspect (float (/ (camera-hsize camera) (camera-vsize camera)))))
    (if (>= aspect 1.0)
        (progn
          (setf (camera-half-width camera) half-view)
          (setf (camera-half-height camera) (div half-view aspect)))
        (progn
          (setf (camera-half-width camera) (mult half-view aspect))
          (setf (camera-half-height camera) half-view)))
    (setf (camera-pixel-size camera)
          (float (/ (mult (camera-half-width camera) 2.0)
                    (camera-hsize camera))))))


(defun create-camera (hsize vsize field-of-view &optional transform)
  (let ((c (make-camera :hsize hsize
                        :vsize vsize
                        :field-of-view field-of-view
                        :transform (if transform
                                       transform
                                       (identity-matrix)))))
    (compute-pixel-size c)
    c))

(defun ray-for-pixel (camera px py)
  (let* ((x-offset (mult (+ px 0.5) (camera-pixel-size camera)))
         (y-offset (mult (+ py 0.5) (camera-pixel-size camera)))
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

(defun render (camera world)
  (let ((image (create-canvas (camera-hsize camera) (camera-vsize camera))))
    (iter (for y from 0 below (camera-vsize camera))
      (iter (for x from 0 below (camera-hsize camera))
        (write-pixel image x y
         (color-at world (ray-for-pixel camera x y)))))
    image))
