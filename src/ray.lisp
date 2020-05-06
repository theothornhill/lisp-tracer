(in-package #:lisp-tracer)

(defmacro with-ray-slots (ray (&rest args) &body body)
  `(with-slots (origin direction) ,ray
     (with-slots ,(mapcar (lambda (o) o) (car args)) origin
         (with-slots ,(mapcar (lambda (d) d) (cadr args)) direction
           ,@body))))

(defmacro with-all-ray-slots (ray &body body)
  "Anaphoric macro to bring all x y z from ray into scope."
  `(with-ray-slots ,ray (((ori-x x) (ori-y y) (ori-z z))
                         ((dir-x x) (dir-y y) (dir-z z)))
     ,@body))

(defun pos (ray time)
  (declare (ray ray) (float time))
  (with-slots (origin direction) ray
    (add origin (mult direction time))))

(defun transform (ray matrix)
  (declare (type ray ray) (type matrix matrix))
  (with-slots (origin direction) ray
    (make-ray :origin (mult matrix origin)
              :direction (mult matrix direction))))
