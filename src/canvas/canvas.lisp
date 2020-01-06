(in-package #:lisp-tracer-canvas)

(defclass canvas ()
  ((width
    :initarg :width
    :accessor width)
   (height
    :initarg :height
    :accessor height)
   (grid
    :initarg :grid
    :accessor grid)))

(defun make-grid (width height)
  (make-array (list width height)
              :element-type 'color
              :initial-element (black)))


(defun canvas! (width height)
  (make-instance 'canvas
                 :width width
                 :height height
                 :grid (make-grid width height)))

(defun write-pixel! (canvas x y col)
  (setf (aref (grid canvas) x y) col))

(defun pixel-at (canvas x y)
  (aref (grid canvas) x y))

(defun color-as-255 (col)
  (cond
    ((>= col 1) 255)
    ((<= col 0) 0)
    (t (round (* 255 col)))))


(defun canvas-to-ppm! (canvas)
  (destructuring-bind (n m) (array-dimensions (grid canvas))
    (with-open-file (str "~/quicklisp/local-projects/lisp-tracer/picture.ppm"
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (format str "P3~%~a ~a~%255" n m)
      (loop for j from 0 below m do
           (format str "~%")
           (loop for i from 0 below n do
                (let ((pixel (pixel-at canvas i j)))
                  (format str
                          "~a ~a ~a "
                          (color-as-255 (red pixel))
                          (color-as-255 (green pixel))
                          (color-as-255 (blue pixel))))))
      (format str "~%"))))
