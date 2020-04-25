(in-package #:lisp-tracer)

(defun create-camera (&key
                        (hsize 1000)
                        (vsize 1000)
                        (field-of-view (/ pi 4))
                        (transform (identity-matrix)))
  (labels ((compute-pixel-size (camera)
             (let ((half-view (tan (/ (camera-field-of-view camera) 2.0)))
                   (aspect (float (/ (camera-hsize camera) (camera-vsize camera)))))
               (if (>= aspect 1.0)
                   (progn
                     (setf (camera-half-width camera) half-view)
                     (setf (camera-half-height camera) (/ half-view aspect)))
                   (progn
                     (setf (camera-half-width camera) (* half-view aspect))
                     (setf (camera-half-height camera) half-view)))
               (setf (camera-pixel-size camera)
                     (/ (* (camera-half-width camera) 2.0)
                        (camera-hsize camera))))))
    (let ((c (make-camera :hsize hsize
                          :vsize vsize
                          :field-of-view field-of-view
                          :transform transform)))
      (compute-pixel-size c)
      c)))

(defun ray-for-pixel (camera px py)
  (declare (type camera camera) (type integer px py))
  (let* ((inverted-camera-transform (inverse (camera-transform camera)))
         (x-offset (mult (+ px 0.5) (camera-pixel-size camera)))
         (y-offset (mult (+ py 0.5) (camera-pixel-size camera)))
         (world-x (sub (camera-half-width camera) x-offset))
         (world-y (sub (camera-half-height camera) y-offset))
         (pixel (mult
                 inverted-camera-transform
                 (make-point :x world-x :y world-y :z -1.0)))
         (origin (mult
                  inverted-camera-transform
                  (make-point)))
         (direction (normalize (sub pixel origin))))
    (make-ray :origin origin :direction direction)))

(defun render (camera world)
  (declare (type camera camera) (type world world))
  (let ((image (create-canvas (camera-hsize camera) (camera-vsize camera))))
    (loop :for y :from 0 :below (camera-vsize camera) :do
      (loop :for x :from 0 :below (camera-hsize camera) :do
        (write-pixel image x y (color-at world (ray-for-pixel camera x y)))))
    image))
