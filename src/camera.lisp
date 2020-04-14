(in-package #:lisp-tracer)

(defun compute-pixel-size (camera)
  (let ((half-view (tan (float (/ (camera-field-of-view camera) 2.0))))
        (aspect (float (/ (camera-hsize camera) (camera-vsize camera)))))
    (if (>= aspect 1.0)
        (progn
          (setf (camera-half-width camera) half-view)
          (setf (camera-half-height camera) (/ half-view aspect)))
        (progn
          (setf (camera-half-width camera) (* half-view aspect))
          (setf (camera-half-height camera) half-view)))
    (setf (camera-pixel-size camera)
          (float (/ (* (camera-half-width camera) 2.0)
                    (camera-hsize camera))))))


(defun create-camera (&key
                        (hsize 1000)
                        (vsize 1000)
                        (field-of-view (/ pi 4))
                        (transform (identity-matrix)))
  (let ((c (make-camera :hsize hsize
                        :vsize vsize
                        :field-of-view field-of-view
                        :transform transform)))
    (compute-pixel-size c)
    c))

(defun ray-for-pixel (camera px py)
  (let* ((x-offset (mult (+ px 0.5) (camera-pixel-size camera)))
         (y-offset (mult (+ py 0.5) (camera-pixel-size camera)))
         (world-x (sub (camera-half-width camera) x-offset))
         (world-y (sub (camera-half-height camera) y-offset))
         (pixel (mult
                 (inverse (camera-transform camera))
                 (make-point :x world-x
                             :y world-y
                             :z -1.0)))
         (origin (mult
                  (inverse (camera-transform camera))
                  (make-point)))
         (direction (normalize (sub pixel origin))))
    (make-ray :origin origin :direction direction)))

(defun render (camera world)
  (let ((image (create-canvas (camera-hsize camera) (camera-vsize camera))))
    (iter (for y from 0 below (camera-vsize camera))
      (iter (for x from 0 below (camera-hsize camera))
        (write-pixel image x y
         (color-at world (ray-for-pixel camera x y)))))
    image))
