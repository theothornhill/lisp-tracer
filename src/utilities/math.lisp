(in-package #:lisp-tracer-utilities)

(defparameter *epsilon* 0.0001)

(defun eq? (a b)
  (< (abs (- a b)) *epsilon*))

(defgeneric add (term1 term2)
  (:documentation "Adding datastructures together")
  (:method ((x number) (y number))
    (+ x y))
  (:method ((x tuple) (y tuple))
    (apply #'make-tuple (maptuple '+ x y)))
  (:method ((x color) (y color))
    (apply #'make-color (mapcolor #'+ x y))))

(defgeneric sub (term1 term2)
  (:documentation "Subtracting datastructures")
  (:method ((x number) (y number))
    (- x y))
  (:method ((x tuple) (y tuple))
    (apply #'make-tuple (maptuple '- x y)))
  (:method ((x color) (y color))
    (apply #'make-color (mapcolor #'- x y))))

(defgeneric mult (term1 term2)
  (:documentation "Multiplying datastructures with scalar")
  (:method ((x number) (y number))
    (* x y))
  (:method ((x tuple) (y number))
    (apply #'make-tuple (mapnumber #'* x y)))
  (:method ((x color) (y number))
    (make-color (* (red x) y)
            (* (green x) y)
            (* (blue x) y)))
  (:method ((x color) (y color))
    (apply #'make-color (mapcolor #'* x y)))
  (:method ((a matrix) (b matrix))
    (let ((size (dimensions a)))
      (make-matrix size (loop :for i :below size :append
                         (loop :for j :below size
                            :collect (+ (* (m a i 0)
                                           (m b 0 j))
                                        (* (m a i 1)
                                           (m b 1 j))
                                        (* (m a i 2)
                                           (m b 2 j))
                                        (* (m a i 3)
                                           (m b 3 j))))))))
  (:method ((a matrix) (b tuple))
    (apply #'make-tuple (loop :for i :below 4
                       :collect (+ (* (m a i 0)
                                      (x b))
                                   (* (m a i 1)
                                      (y b))
                                   (* (m a i 2)
                                      (z b))
                                   (* (m a i 3)
                                      (w b)))))))

(defgeneric div (term1 term2)
  (:documentation "Dividing datastructures with scalar")
  (:method ((x number) (y number))
    (float (/ x y)))
  (:method ((x tuple) (y number))
    (->> (mapnumber #'/ x y)
         (map 'list #'float)
         (apply #'make-tuple))))

(defgeneric equal? (a b)
  (:documentation "Checking for unsafe equality")
  (:method ((a number) (b number))
    (eq? a b))
  (:method ((x tuple) (y tuple))
    (and (eq? (x x) (x y))
         (eq? (y x) (y y))
         (eq? (z x) (z y))
         (eq? (w x) (w y))))
  (:method ((x color) (y color))
    (and (eq? (red x) (red y))
         (eq? (green x) (green y))
         (eq? (blue x) (blue y))))
  (:method ((x matrix) (y matrix))
    (loop :for i :below (dimensions x) :always
         (loop :for j :below (dimensions x) :always
              (eq? (m x i j) (m y i j))))))

(defgeneric neg (element)
  (:documentation "Negates provided value")
  (:method ((x number))
    (- x))
  (:method ((tup tuple))
    (make-tuple (- (x tup))
            (- (y tup))
            (- (z tup))
            (- (w tup)))))

(defun magnitude (vec)
  (let ((lst (tuple-to-list vec)))
    (->> lst
     (map 'list (lambda (x) (* x x)))
     (reduce '+ )
     (sqrt))))

(defun normalize (vec)
  (make-tuple (div (x vec) (magnitude vec))
          (div (y vec) (magnitude vec))
          (div (z vec) (magnitude vec))
          (div (w vec) (magnitude vec))))

(defun dot (a b)
  (let ((tuple-a (tuple-to-list a))
        (tuple-b (tuple-to-list b)))
   (->> (map 'list #'list tuple-a tuple-b)
        (map 'list #'(lambda (x) (reduce #'* x)))
        (reduce #'+))))

(defun cross (a b)
  (make-vec (- (* (y a) (z b))
           (* (z a) (y b)))
        (- (* (z a) (x b))
           (* (x a) (z b)))
        (- (* (x a) (y b))
           (* (y a) (x b)))))
