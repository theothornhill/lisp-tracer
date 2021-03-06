(in-package :lisp-tracer-tests)

(defun read-file-as-lines (filename)
  "Read file into a list of lines."
  (with-open-file (in filename)
    (loop
      :for line = (read-line in nil nil)
      :while line
      :collect line)))

(deftest canvas-test
  (testing "Creating a canvas"
    (let ((c (create-canvas 10 20)))
      (ok (equal? (canvas-width c) 10))
      (ok (equal? (canvas-height c) 20)))))

(deftest writing-to-canvas
  (testing "Writing pixels to a canvas"
    (let ((c (create-canvas 10 20))
          (color-red (make-color :red 1.0 :green 0.0 :blue 0.0)))
      (write-pixel c 2 3 color-red)
      (ok (equal? (pixel-at c 2 3) color-red)))))

(deftest ppm
  (testing "Constructing PPM header"
    (let ((c (create-canvas 5 3)))
      (canvas-to-ppm c)
      (let ((lines (read-file-as-lines "/home/theodor/quicklisp/local-projects/lisp-tracer/picture.ppm")))
        (ok (string= (car lines) "P3"))
        (ok (string= (cadr lines) "5 3"))
        (ok (string= (caddr lines) "255"))))))

(deftest ppm-pixel-data
  (testing "Constructing the PPM pixel data"
    (let* ((c (create-canvas 5 3))
          (c1 (make-color :red 1.5 :green 0.0 :blue 0.0))
          (c2 (make-color :red 0.0 :green 0.5 :blue 0.0))
          (c3 (make-color :red -0.5 :green 0.0 :blue 1.0)))
      (write-pixel c 0 0 c1)
      (write-pixel c 2 1 c2)
      (write-pixel c 4 2 c3)
      (canvas-to-ppm c)
      (let ((lines (read-file-as-lines "/home/theodor/quicklisp/local-projects/lisp-tracer/picture.ppm")))
        (ok (string= (fourth lines) "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "))
        (ok (string= (fifth lines) "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0 "))
        (ok (string= (sixth lines) "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255 "))
        (ng (seventh lines))))))
