(in-package #:lisp-tracer)

(defun create-canvas (width height)
  (make-canvas
   :width width
   :height height
   :grid (make-array (list width height)
                     :element-type 'color
                     :initial-element (make-color))))

(defun write-pixel (canvas x y color)
  (setf (aref (canvas-grid canvas) x y)
        color))

(defun pixel-at (canvas x y)
  "Return color on a given pixel x y."
  (aref (canvas-grid canvas) x y))

(defun color-as-255 (color)
  "Return color values in a color converted to RGB 0-255 range."
  (declare (type color color))
  (labels ((convert-color (col)
             (cond
               ((>= col 1) 255)
               ((<= col 0) 0)
               (t (round (* 255 col))))))
    (values (convert-color (color-red color))
            (convert-color (color-green color))
            (convert-color (color-blue color)))))

(defun canvas-to-ppm (canvas)
  "Write the actual canvas to a PPM file."
  (destructuring-bind (n m) (array-dimensions (canvas-grid canvas))
    (with-open-file (str "/home/theodor/quicklisp/local-projects/lisp-tracer/picture.ppm"
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (format str "P3~%~a ~a~%255" n m)
      (iterate (for j from 0 below m)
        (format str "~%")
        (iterate (for i from 0 below n)
          (let ((color (pixel-at canvas i j)))
            (multiple-value-bind (red green blue) (color-as-255 color)
              (format str "~a ~a ~a " red green blue)))))
      (format str "~%"))))
