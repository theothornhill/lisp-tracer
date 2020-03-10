(in-package #:lisp-tracer)

(defun maptuple (op tuple-a tuple-b)
  (list (funcall op (x tuple-a) (x tuple-b))
        (funcall op (y tuple-a) (y tuple-b))
        (funcall op (z tuple-a) (z tuple-b))
        (funcall op (w tuple-a) (w tuple-b))))

(defun mapnumber (op tuple-a number)
  (list (funcall op (x tuple-a) number)
        (funcall op (y tuple-a) number)
        (funcall op (z tuple-a) number)
        (funcall op (w tuple-a) number)))

(defun mapcolor (op a b)
  (list (funcall op (red a) (red b))
        (funcall op (green a) (green b))
        (funcall op (blue a) (blue b))))

(defun tuple-to-list (tuple)
  (list (x tuple) (y tuple) (z tuple) (w tuple)))

(defmacro transform-object (&rest transforms)
  `(reduce #'mult (list ,@(reverse transforms))))

(defun reflect (in normal)
  (declare (tuple in) (tuple normal))
  (sub in (reduce #'mult (list normal 2 (dot in normal)))))
