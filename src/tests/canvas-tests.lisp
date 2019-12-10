(in-package #:lisp-tracer-tests)

(defun read-file-as-lines (filename)
  "Read file into a list of lines."
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
      while line
      collect line)))

(defun line-as-list (line)
  "Read all objects from the line as a list."
  (read-from-string (concatenate 'string "(" line ")")))

(defun lines-as-list ()
  (mapcar #'line-as-list (read-file-as-lines "../../picture.ppm")))


(deftest canvas-test
  (testing "Creating a canvas"
    (let ((c (canvas! 10 20)))
      (ok (equal? (width c) 10))
      (ok (equal? (height c) 20)))))

(deftest writing-to-canvas
  (testing "Writing pixels to a canvas"
    (let ((c (canvas! 10 20))
          (red (color! 1 0 0)))
      (write-pixel! c 2 3 red)
      (ok (equal? (pixel-at c 2 3) red)))))

(deftest ppm
  (testing "Constructing PPM header"
    (let ((c (canvas! 5 3)))
      (canvas-to-ppm! c)
      (let ((lines (lines-as-list)))
        (ok (equal (first lines) (list 'P3)))
        (ok (equal (second lines) (list 5 3)))
        (ok (equal (third lines) (list 255)))))))

(deftest ppm-pixel-data
  (testing "Constructing the PPM pixel data"
    (let ((c (canvas! 5 3))
          (c1 (color! 1.5 0 0))
          (c2 (color! 0 0.5 0))
          (c3 (color! -0.5 0 1)))
      (write-pixel! c 0 0 c1)
      (write-pixel! c 2 1 c2)
      (write-pixel! c 4 2 c3)
      (canvas-to-ppm! c)
      (let ((lines (lines-as-list)))
        (ok (equal (fourth lines) (list 255 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
        (ok (equal (fifth lines) (list 0 0 0 0 0 0 0 128 0 0 0 0 0 0 0)))
        (ok (equal (sixth lines) (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 255)))
        (ng (seventh lines))))))


(run-suite *package*)
