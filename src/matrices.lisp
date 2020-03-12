(in-package #:lisp-tracer)

(declaim (inline partition))
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
  (let ((grid (grid matrix)))
    (make-matrix
     4
     `(,(aref grid 0 0)
       ,(aref grid 1 0)
       ,(aref grid 2 0)
       ,(aref grid 3 0)
       
       ,(aref grid 0 1)
       ,(aref grid 1 1)
       ,(aref grid 2 1)
       ,(aref grid 3 1)
       
       ,(aref grid 0 2)
       ,(aref grid 1 2)
       ,(aref grid 2 2)
       ,(aref grid 3 2)
       
       ,(aref grid 0 3)
       ,(aref grid 1 3)
       ,(aref grid 2 3)
       ,(aref grid 3 3)))))

(declaim (inline ab-cd))
(defun ab-cd (a b c d)
  "Helper function for inversion of matrix."
  (- (* a b) (* c d)))

(declaim (inline +inverted-cell))
(defun +inverted-cell (x y z one two three)
  "Helper function for inversion of matrix."
  (+ (+ (- (* x one)
           (* y two))
        (* z three))))

(declaim (inline -inverted-cell))
(defun -inverted-cell (x y z one two three)
  "Helper function for inversion of matrix."
  (- (+ (- (* x one)
           (* y two))
        (* z three))))

(defun inverse (matrix)
  "Invert MATRIX if it is INVERTIBLE."
  (let* ((grid (grid matrix))
         (a (aref grid 0 0))
         (b (aref grid 0 1))
         (c (aref grid 0 2))
         (d (aref grid 0 3))

         (e (aref grid 1 0))
         (f (aref grid 1 1))
         (g (aref grid 1 2))
         (h (aref grid 1 3))

         (i (aref grid 2 0))
         (j (aref grid 2 1))
         (k (aref grid 2 2))
         (l (aref grid 2 3))

         (m (aref grid 3 0))
         (n (aref grid 3 1))
         (o (aref grid 3 2))
         (p (aref grid 3 3))

         (kp-lo (ab-cd k p l o))
         (jp-ln (ab-cd j p l n))
         (jo-kn (ab-cd j o k n))
         (ip-lm (ab-cd i p l m))
         (io-km (ab-cd i o k m))
         (in-jm (ab-cd i n j m))

         (a11 (+inverted-cell f g h kp-lo jp-ln jo-kn))
         (a12 (-inverted-cell e g h kp-lo ip-lm io-km))
         (a13 (+inverted-cell e f h jp-ln ip-lm in-jm))
         (a14 (-inverted-cell e f g jo-kn io-km in-jm))
         (det (+ (* a a11)
                 (* b a12)
                 (* c a13)
                 (* d a14))))
    (if (< (abs det) epsilon)
        (error "This matrix is not invertible")
        (let ((inv-det (/ 1.0 det))
              (gp-ho (ab-cd g p h o))
              (fp-hn (ab-cd f p h n))
              (fo-gn (ab-cd f o g n))
              (ep-hm (ab-cd e p h m))
              (eo-gm (ab-cd e o g m))
              (en-fm (ab-cd e n f m))
              (gl-hk (ab-cd g l h k))
              (fl-hj (ab-cd f l h j))
              (fk-gj (ab-cd f k g j))
              (el-hi (ab-cd e l h i))
              (ek-gi (ab-cd e k g i))
              (ej-fi (ab-cd e j f i)))
          (make-matrix
           4
           `(,(* a11 inv-det)
             ,(* (-inverted-cell b c d kp-lo jp-ln jo-kn) inv-det)
             ,(* (+inverted-cell b c d gp-ho fp-hn fo-gn) inv-det)
             ,(* (-inverted-cell b c d gl-hk fl-hj fk-gj) inv-det)

             ,(* a12 inv-det)
             ,(* (+inverted-cell a c d kp-lo ip-lm io-km) inv-det)
             ,(* (-inverted-cell a c d gp-ho ep-hm eo-gm) inv-det)
             ,(* (+inverted-cell a c d gl-hk el-hi ek-gi) inv-det)

             ,(* a13 inv-det)
             ,(* (-inverted-cell a b d jp-ln ip-lm in-jm) inv-det)
             ,(* (+inverted-cell a b d fp-hn ep-hm en-fm) inv-det)
             ,(* (-inverted-cell a b d fl-hj el-hi ej-fi) inv-det)

             ,(* a14 inv-det)
             ,(* (+inverted-cell a b c jo-kn io-km in-jm) inv-det)
             ,(* (-inverted-cell a b c fo-gn eo-gm en-fm) inv-det)
             ,(* (+inverted-cell a b c fk-gj ek-gi ej-fi) inv-det)))))))


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
