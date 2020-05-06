(in-package #:lisp-tracer)

(declaim (inline make-intersection))
(defun make-intersection (&key (tt 0.0) (object nil))
  "Creates an intersection with an OBJECT at a given time TT."
  (make-rt-intersection :tt tt :object object))

(defmacro set-new-refractive (n containers)
  "Helper for update-n"
  `(setf ,n (material-refractive-index
             (shape-material (car (last ,containers))))))

(defmacro update-n (n)
  "Helper for refractive indexes. Anaphoric - uses variable capture"
  `(when (eq i hit)
     (if containers
         (set-new-refractive ,n containers)
         (setf ,n 1.0))))

(defun refractive-indexes (hit is)
  (declare (type rt-intersection hit) (type cons is))
  (let ((n1 1.0) (n2 1.0) containers)
    (mapc (lambda (i)
            (update-n n1)
            (with-slots (object) i
              (if (member object containers :test 'eq)
                  (setf containers (remove object containers))
                  (setf containers (append containers (list object)))))
            (update-n n2))
          is)
    (values n1 n2)))

(defun schlick (comps)
  (with-slots (n1 n2 eyev normalv) comps
    (let ((cos (dot eyev normalv)))
      (when (> n1 n2)
        (let* ((n (/ n1 n2))
               (sin2-t (* (expt n 2) (- 1.0 (expt cos 2)))))
          (when (> sin2-t 1.0) (return-from schlick 1.0))
          (setf cos (sqrt (- 1.0 sin2-t)))))
      (let ((r0 (expt (/ (- n1 n2) (+ n1 n2)) 2)))
        (+ r0 (* (- 1 r0) (expt (- 1 cos) 5)))))))

(defun prepare-computations (i r &optional (xs (list i)))
  (declare (type rt-intersection i) (type ray r) (type list xs))
  (with-slots (tt object) i
    (with-slots (direction) r
      (let* ((comps-point (pos r tt))
             (comps-eyev (neg direction))
             (comps-normalv (normal-at object comps-point))
             (inside? (< (dot comps-normalv comps-eyev) 0.0))
             (normalv (if inside? (neg comps-normalv) comps-normalv))
             (reflectv (reflect direction normalv))
             (comps-over-point (add comps-point (mult normalv epsilon)))
             (comps-under-point (sub comps-point (mult normalv epsilon))))
        (multiple-value-bind (n1 n2) (refractive-indexes i xs)
          (make-computations
           :tt tt
           :n1 n1
           :n2 n2
           :object object
           :inside? inside?
           :point comps-point
           :over-point comps-over-point
           :under-point comps-under-point
           :eyev comps-eyev
           :normalv normalv
           :reflectv reflectv))))))

(declaim (inline tt<))
(defun tt< (a b)
  (< (rt-intersection-tt a) (rt-intersection-tt b)))

(defun intersections (&rest xs)
  "Sorted list of intersections. Compares by TT from smallest to largest."
  (if xs (sort xs #'tt<)))

(defun intersect (shape ray)
  (with-slots (transform) shape
    (let ((local-ray (transform ray (inverse transform))))
      (local-intersect shape local-ray))))

(defun hit (xs)
  "If HIT is found with TT > 0, return the HIT"
  (declare (type list xs))
  (cond ((null xs) nil)
        ((> (rt-intersection-tt (car xs)) 0) (car xs))
        (t (hit (cdr xs)))))

(defun intersect-world (world ray)
  (with-slots (objects) world
    (sort (mapcan #'(lambda (s) (intersect s ray)) objects) #'tt<)))
