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

(defmethod print-object ((obj matrix) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((grid grid))
        obj
      (format stream "~a" grid))))

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

(defun determinant2x2 (matrix)
  (- (* (m matrix 0 0)
        (m matrix 1 1))
     (* (m matrix 0 1)
        (m matrix 1 0))))

(defun determinant (matrix)
  (let ((det 0))
    (cond
      ((eq (dimensions matrix) 2)
       (setf det (determinant2x2 matrix)))
      (t (loop :for column :below (dimensions matrix) do
              (setf det (+ det (* (m matrix 0 column)
                                  (cofactor matrix 0 column)))))))
    det))

(defun submatrix (matrix x y)
  (let ((size (dimensions matrix)))
    (matrix! (1- size) (loop :for i :below size :append
                            (loop :for j :below size
                               :unless (or (= i x) (= j y))
                                 :collect (m matrix i j))))))

(defun minor (matrix i j)
  (determinant (submatrix matrix i j)))

(defun cofactor (matrix i j)
  (let ((minor (minor matrix i j)))
    (if (oddp (+ i j)) (- minor) minor)))

(defun invertible? (matrix)
  (not (equal (determinant matrix) 0)))

(defun inverse (matrix)
  (unless (invertible? matrix)
    (error "error here mister"))
  (let ((m2 (identity-matrix))
        (size (dimensions matrix)))
    (loop :for row :below size :do
      (loop :for column :below size :do
            (let ((c (cofactor matrix row column)))
              (setf (aref (grid m2) column row)
                    (/ c (determinant matrix))))))
    m2))

(defun translation (x y z)
  (matrix! 4 `(1  0  0 ,x
               0  1  0 ,y
               0  0  1 ,z
               0  0  0  1)))

(defun scaling (x y z)
  (matrix! 4 `(,x  0  0  0
                0 ,y  0  0 
                0  0 ,z  0
                0  0  0  1)))
