(in-package #:lisp-tracer)

(defun partition (size list)
  "Partition a list in chunks of SIZE size."
  (labels ((part (list) (nthcdr size list)))
    (iter (for sublist :on list :by #'part)
      (collect (subseq sublist 0 size)))))

(defclass matrix ()
  ((dimensions
    :initarg :dimensions
    :accessor dimensions
    :documentation "Dimension of a matrix.
Shown only by one number, since we only deal with square matrices.")
   (grid
    :initarg :grid
    :accessor grid
    :documentation "The actual matrix."))
  (:documentation "A square MATRIX of size 2x2, 3x3 or 4x4."))


(defun make-matrix (dimensions list)
  "Create a matrix of size DIMENSION and items in LIST"
  (declare (fixnum dimensions) (list list))
  (make-instance
   'matrix
   :dimensions dimensions
   :grid (make-array (list dimensions dimensions)
                     :initial-contents
                     (partition dimensions list))))

(defun m (matrix i j)
  "Getter for cell in MATRIX at index I J."
  (declare (matrix matrix) (integer i) (integer j))
  (aref (grid matrix) i j))

(defun identity-matrix ()
  "IDENTITY-MATRIX - 4x4."
  (make-matrix
   4
   '(1 0 0 0
     0 1 0 0
     0 0 1 0
     0 0 0 1)))

(defun transpose (matrix)
  "Flips a MATRIX along its diagonal."
  (declare (matrix matrix))
  (make-matrix
   4
   (iter (for i below 4)
     (appending
      (iter (for j below 4)
        (collect (m matrix j i)))))))

(defun determinant2x2 (matrix)
  "Calculate the determinant of a 2x2 MATRIX."
  (declare (matrix matrix))
  (- (* (m matrix 0 0)
        (m matrix 1 1))
     (* (m matrix 0 1)
        (m matrix 1 0))))

(defun determinant (matrix)
  "Calculate determinant of a MATRIX."
  (declare (matrix matrix))
  (let ((det 0))
    (cond
      ((eq (dimensions matrix) 2)
       (setf det (determinant2x2 matrix)))
      (t (iter (for column below (dimensions matrix))
           (setf det (+ det (* (m matrix 0 column)
                               (cofactor matrix 0 column)))))))
    det))

(defun submatrix (matrix x y)
  "Returns a new MATRIX with row X and column Y removed."
  (declare (matrix matrix) (integer x) (integer y))
  (let ((size (dimensions matrix)))
    (make-matrix
     (1- size)
     (iter (for i below size)
       (appending
        (iter (for j below size)
          (unless (or (= i x) (= j y))
            (collect (m matrix i j)))))))))

(defun minor (matrix i j)
  "Return the DETERMINANT of the SUBMATRIX at I and j."
  (declare (matrix matrix) (integer i) (integer j))
  (determinant (submatrix matrix i j)))

(defun cofactor (matrix i j)
  "Computes COFACTOR of a MATRIX. If I + J is odd, then negate value."
  (declare (matrix matrix) (integer i) (integer j))
  (let ((minor (minor matrix i j)))
    (if (oddp (+ i j)) (- minor) minor)))

(defun invertible? (matrix)
  "If determinant != 0 then MATRIX is invertible."
  (declare (matrix matrix))
  (not (equal (determinant matrix) 0)))

(defun inverse (matrix)
  "Invert MATRIX if it is INVERTIBLE."
  (unless (invertible? matrix)
    (error "This matrix is not invertible"))
  (let ((m2 (identity-matrix))
        (size (dimensions matrix)))
    (iter (for row below size)
      (iter (for column below size)
        (let ((c (cofactor matrix row column)))
          (setf (aref (grid m2) column row)
                (/ c (determinant matrix))))))
    m2))

(defun translation (x y z)
  "Translate a MATRIX using its W component."
  (make-matrix 4 `(1  0  0 ,x
                   0  1  0 ,y
                   0  0  1 ,z
                   0  0  0  1)))

(defun scaling (x y z)
  "Scale a MATRIX."
  (make-matrix 4 `(,x  0  0  0
                    0 ,y  0  0
                    0  0 ,z  0
                    0  0  0  1)))

(defun rotation-x (r)
  "Rotate a MATRIX along its X-axis by R radians."
  (make-matrix 4 `(1 0        0            0
                   0 ,(cos r) ,(- (sin r)) 0
                   0 ,(sin r) ,(cos r)     0
                   0 0        0            1)))

(defun rotation-y (r)
  "Rotate a MATRIX along its Y-axis by R radians."
  (make-matrix 4 `(,(cos r) 0            ,(sin r)    0
                   0        1            0           0
                   0        ,(- (sin r)) ,(cos r)    0
                   0        0            0           1)))

(defun rotation-z (r)
  "Rotate a MATRIX along its Z-axis by R radians."
  (make-matrix 4 `(,(cos r) ,(- (sin r)) 0 0
                   ,(sin r) ,(cos r)     0 0
                   0        0            1 0
                   0        0            0 1)))

(defun shearing (q w e a s d)
  "Shear a MATRIX."
  (make-matrix 4 `(1  ,q ,w 0
                   ,e 1  ,a 0
                   ,s ,d 1  0
                   0  0  0  1)))


(defmethod print-object ((obj matrix) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((grid grid))
        obj
      (format stream "~a" grid))))
