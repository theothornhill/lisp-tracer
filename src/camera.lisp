(in-package #:lisp-tracer)

(defun create-camera (&key
                        (hsize 1000)
                        (vsize 1000)
                        (field-of-view (/ pi 4))
                        (transform (identity-matrix)))
  (labels ((compute-pixel-size (camera)
             (with-slots (pixel-size half-width half-height hsize vsize field-of-view)
                 camera
               (let ((half-view (tan (/ field-of-view 2.0)))
                     (aspect (float (/ hsize vsize))))
                 (if (>= aspect 1.0)
                     (progn
                       (setf half-width half-view)
                       (setf half-height (/ half-view aspect)))
                     (progn
                       (setf half-width (* half-view aspect))
                       (setf half-height half-view)))
                 (setf pixel-size (/ (* half-width 2.0) hsize))))))
    (let ((c (make-camera :hsize hsize
                          :vsize vsize
                          :field-of-view field-of-view
                          :transform transform)))
      (compute-pixel-size c)
      c)))

(defun ray-for-pixel (camera px py)
  (declare (type camera camera) (type integer px py))
  (with-slots (pixel-size transform half-width half-height) camera
    (let* ((inverted-camera-transform (inverse transform))
           (x-offset (mult (+ px 0.5) pixel-size))
           (y-offset (mult (+ py 0.5) pixel-size))
           (world-x (sub half-width x-offset))
           (world-y (sub half-height y-offset))
           (pixel (mult
                   inverted-camera-transform
                   (make-point :x world-x :y world-y :z -1.0)))
           (origin (mult
                    inverted-camera-transform
                    (make-point)))
           (direction (normalize (sub pixel origin))))
      (make-ray :origin origin :direction direction))))

(defun render (camera world)
  (declare (type camera camera) (type world world))
  (let ((image (create-canvas (camera-hsize camera) (camera-vsize camera))))
    (loop :for y :from 0 :below (camera-vsize camera) :do
      (loop :for x :from 0 :below (camera-hsize camera) :do
        (write-pixel image x y (color-at world (ray-for-pixel camera x y)))))
    image))
