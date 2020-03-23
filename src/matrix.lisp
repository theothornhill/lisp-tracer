(in-package #:lisp-tracer)

(defstruct matrix
  (m00 1f0 :type single-float)
  (m01 0f0 :type single-float)
  (m02 0f0 :type single-float)
  (m03 0f0 :type single-float)
  (m10 0f0 :type single-float)
  (m11 1f0 :type single-float)
  (m12 0f0 :type single-float)
  (m13 0f0 :type single-float)
  (m20 0f0 :type single-float)
  (m21 0f0 :type single-float)
  (m22 1f0 :type single-float)
  (m23 0f0 :type single-float)
  (m30 0f0 :type single-float)
  (m31 0f0 :type single-float)
  (m32 0f0 :type single-float)
  (m33 1f0 :type single-float))

(declaim (inline create-matrix))
(declaim (inline make-matrix))
(defun create-matrix (a b c d
                      e f g h
                      i j k l
                      m n o p)
  "Create a matrix of size DIMENSION and items in LIST"
  (declare (optimize (speed 3) (safety 0)))
  (make-matrix
   :m00 a
   :m01 b
   :m02 c
   :m03 d
   :m10 e
   :m11 f
   :m12 g
   :m13 h
   :m20 i
   :m21 j
   :m22 k
   :m23 l
   :m30 m
   :m31 n
   :m32 o
   :m33 p))

(defun identity-matrix ()
  "IDENTITY-MATRIX - 4x4."
  (create-matrix
   1f0 0f0 0f0 0f0
   0f0 1f0 0f0 0f0
   0f0 0f0 1f0 0f0
   0f0 0f0 0f0 1f0))

(defun transpose (matrix)
  "Flips a MATRIX along its diagonal."
  (declare (matrix matrix))
  (make-matrix
   :m00 (matrix-m00 matrix)
   :m01 (matrix-m10 matrix)
   :m02 (matrix-m20 matrix)
   :m03 (matrix-m30 matrix)
   :m10 (matrix-m01 matrix)
   :m11 (matrix-m11 matrix)
   :m12 (matrix-m21 matrix)
   :m13 (matrix-m31 matrix)
   :m20 (matrix-m02 matrix)
   :m21 (matrix-m12 matrix)
   :m22 (matrix-m22 matrix)
   :m23 (matrix-m32 matrix)
   :m30 (matrix-m03 matrix)
   :m31 (matrix-m13 matrix)
   :m32 (matrix-m23 matrix)
   :m33 (matrix-m33 matrix)))

(declaim (inline ab-cd))
(defun ab-cd (a b c d)
  "Helper function for inversion of matrix."
  (declare (type single-float a b c d))
  (- (* a b) (* c d)))

(declaim (inline +inverted-cell))
(defun +inverted-cell (x y z one two three)
  "Helper function for inversion of matrix."
  (declare (type single-float x y z one two three))
  (+ (+ (- (* x one)
           (* y two))
        (* z three))))

(declaim (inline -inverted-cell))
(defun -inverted-cell (x y z one two three)
  "Helper function for inversion of matrix."
  (declare (type single-float x y z one two three))
  (- (+ (- (* x one)
           (* y two))
        (* z three))))

(declaim (inline inverse))
(defun inverse (matrix)
  "Invert MATRIX if it is INVERTIBLE."
  (declare (optimize (speed 3) (safety 0)))
  (let* ((a (matrix-m00 matrix))
         (b (matrix-m01 matrix))
         (c (matrix-m02 matrix))
         (d (matrix-m03 matrix))

         (e (matrix-m10 matrix))
         (f (matrix-m11 matrix))
         (g (matrix-m12 matrix))
         (h (matrix-m13 matrix))

         (i (matrix-m20 matrix))
         (j (matrix-m21 matrix))
         (k (matrix-m22 matrix))
         (l (matrix-m23 matrix))

         (m (matrix-m30 matrix))
         (n (matrix-m31 matrix))
         (o (matrix-m32 matrix))
         (p (matrix-m33 matrix))

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
    (declare (type single-float epsilon))
    (if (< (abs det) epsilon)
        (error "This matrix is not invertible")
        (let ((inv-det (/ 1f0 det))
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
          (declare (type single-float inv-det))
          (make-matrix
           :m00 (* a11 inv-det)
           :m01 (* (-inverted-cell b c d kp-lo jp-ln jo-kn) inv-det)
           :m02 (* (+inverted-cell b c d gp-ho fp-hn fo-gn) inv-det)
           :m03 (* (-inverted-cell b c d gl-hk fl-hj fk-gj) inv-det)
           :m10 (* a12 inv-det)
           :m11 (* (+inverted-cell a c d kp-lo ip-lm io-km) inv-det)
           :m12 (* (-inverted-cell a c d gp-ho ep-hm eo-gm) inv-det)
           :m13 (* (+inverted-cell a c d gl-hk el-hi ek-gi) inv-det)
           :m20 (* a13 inv-det)
           :m21 (* (-inverted-cell a b d jp-ln ip-lm in-jm) inv-det)
           :m22 (* (+inverted-cell a b d fp-hn ep-hm en-fm) inv-det)
           :m23 (* (-inverted-cell a b d fl-hj el-hi ej-fi) inv-det)
           :m30 (* a14 inv-det)
           :m31 (* (+inverted-cell a b c jo-kn io-km in-jm) inv-det)
           :m32 (* (-inverted-cell a b c fo-gn eo-gm en-fm) inv-det)
           :m33 (* (+inverted-cell a b c fk-gj ek-gi ej-fi) inv-det))))))


(defun translation (x y z)
  "Translate a MATRIX using its W component."
  (declare (optimize (speed 3) (safety 0)))
  (create-matrix
   1f0 0f0 0f0 x
   0f0 1f0 0f0 y
   0f0 0f0 1f0 z
   0f0 0f0 0f0 1f0))

(defun scaling (x y z)
  "Scale a MATRIX."
  (declare (optimize (speed 3) (safety 0)))
  (create-matrix
   x 0f0 0f0 0f0
   0f0 y 0f0 0f0
   0f0 0f0 z 0f0
   0f0 0f0 0f0 1f0))

(defun rotation-x (r)
  "Rotate a MATRIX along its X-axis by R radians."
  (declare (optimize (speed 3) (safety 0))
           (type single-float r))
  (let ((cos-r (coerce (cos r) 'single-float))
        (sin-r (coerce (sin r) 'single-float)))
    (create-matrix
     1f0 0f0 0f0 0f0
     0f0 cos-r (- sin-r) 0f0
     0f0 sin-r cos-r 0f0
     0f0 0f0 0f0 1f0)))

(defun rotation-y (r)
  "Rotate a MATRIX along its Y-axis by R radians."
  (declare (optimize (speed 3) (safety 0))
           (type single-float r))
  (let ((cos-r (coerce (cos r) 'single-float))
        (sin-r (coerce (sin r) 'single-float)))
    (create-matrix
     cos-r 0f0 sin-r 0f0
     0f0 1f0 0f0 0f0
     (- sin-r) 0f0 cos-r 0f0
     0f0 0f0 0f0 1f0)))

(defun rotation-z (r)
  "Rotate a MATRIX along its Z-axis by R radians."
  (declare (optimize (speed 3) (safety 0))
           (type single-float r))
  (let ((cos-r (coerce (cos r) 'single-float))
        (sin-r (coerce (sin r) 'single-float)))
    (create-matrix
     cos-r (- sin-r) 0f0 0f0
     sin-r cos-r 0f0 0f0
     0f0 0f0 1f0 0f0
     0f0 0f0 0f0 1f0)))

(defun shearing (q w e a s d)
  "Shear a MATRIX."
  (declare (optimize (speed 3) (safety 0)))
  (create-matrix
   1f0 q w 0f0
   e 1f0 a 0f0
   s d 1f0 0f0
   0f0 0f0 0f0 1f0))

(defun view-transform (from to up)
  (declare (type tuple from to up))
  (let* ((forward (normalize (sub to from)))
         (upn (normalize up))
         (left (cross forward upn))
         (true-up (cross left forward))
         (negated-forward (neg forward))
         (negated-from (neg from))
         (orientation (create-matrix
                       (tuple-x left)
                       (tuple-y left)
                       (tuple-z left)
                       0.0
                       (tuple-x true-up)
                       (tuple-y true-up)
                       (tuple-z true-up)
                       0.0
                       (tuple-x negated-forward)
                       (tuple-y negated-forward)
                       (tuple-z negated-forward)
                       0.0
                       0.0
                       0.0
                       0.0
                       1.0)))
    (mult orientation (translation (tuple-x negated-from)
                                   (tuple-y negated-from)
                                   (tuple-z negated-from)))))
