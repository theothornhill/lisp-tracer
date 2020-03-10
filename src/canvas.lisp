(in-package #:lisp-tracer)

(defclass canvas ()
  ((width
    :initarg :width
    :accessor width
    :documentation "Width of the canvas.")
   (height
    :initarg :height
    :accessor height
    :documentation "Height of the canvas.")
   (canvas
    :initarg :canvas
    :accessor canvas
    :documentation "The canvas itself. A grid of size WIDTH x HEIGHT."))
  (:documentation "A CANVAS is a grid of COLOR."))

(defun make-grid (width height)
  "Array of WIDTH and HEIGHT size filled with BLACK."
  (make-array (list width height)
              :element-type 'color
              :initial-element (black)))


(defun make-canvas (width height)
  "CANVAS of WIDTH and HEIGHT size with a supplied grid of colors."
  (make-instance 'canvas
                 :width width
                 :height height
                 :canvas (make-grid width height)))

(defun write-pixel (canvas x y color)
  "Set a cell in the CANVAS grid with COLOR at position X Y."
  (setf (aref (canvas canvas) x y) color))

(defun pixel-at (canvas x y)
  "Get the pixel in CANVAS at position X Y."
  (aref (canvas canvas) x y))

(defun color-as-255 (col)
  "Calculate the color as an integer from 0 to 255. Don't allow larger
values than 255 or smaller values than 0."
  (cond
    ((>= col 1) 255)
    ((<= col 0) 0)
    (t (round (* 255 col)))))


(defun canvas-to-ppm (canvas)
  "Write the actual canvas to a PPM file."
  (destructuring-bind (n m) (array-dimensions (canvas canvas))
    (with-open-file (str "/home/theodor/quicklisp/local-projects/lisp-tracer/picture.ppm"
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (format str "P3~%~a ~a~%255" n m)
      (iterate (for j from 0 below m)
        (format str "~%")
        (iterate (for i from 0 below n)
          (let ((pixel (pixel-at canvas i j)))
            (format str
                    "~a ~a ~a "
                    (color-as-255 (red pixel))
                    (color-as-255 (green pixel))
                    (color-as-255 (blue pixel))))))
      (format str "~%"))))
