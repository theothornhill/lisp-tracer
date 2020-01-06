(in-package #:lisp-tracer-matrices)


(defun partition (size list)
  (labels ((part (list) (nthcdr size list)))
    (loop :for sublist :on list :by #'part
                :collect (subseq sublist 0 size))))

(defclass matrix ()
  ((dimensions
    :documentation "Size of matrix - 2x2, 3x3 or 4x4 are used here"
    :initarg :dimensions
    :accessor dimensions)
   (grid
    :documentation "The actual matrix"
    :initarg :grid
    :accessor grid)))

(defun matrix! (dimensions list)
  (make-instance 'matrix
                 :dimensions dimensions
                 :grid (make-array (list dimensions dimensions)
                                   :initial-contents
                                   (partition dimensions list))))

(defun m (matrix i j)
  "Getter for matrix at index i j"
  (aref (grid matrix) i j))

(defun identity-matrix ()
  (matrix! 4 '(1 0 0 0
               0 1 0 0
               0 0 1 0
               0 0 0 1)))

(defun transpose (matrix)
  (matrix! 4 (loop :for i :below 4 :append
                  (loop :for j :below 4 :collect
                       (m matrix j i)))))

(defun determinant (matrix)
  (- (* (m matrix 0 0)
        (m matrix 1 1))
     (* (m matrix 0 1)
        (m matrix 1 0))))
