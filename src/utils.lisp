(in-package #:lisp-tracer)

(defun maptuple (op tuple-a tuple-b)
  (list (funcall op (tuple-x tuple-a) (tuple-x tuple-b))
        (funcall op (tuple-y tuple-a) (tuple-y tuple-b))
        (funcall op (tuple-z tuple-a) (tuple-z tuple-b))
        (funcall op (tuple-w tuple-a) (tuple-w tuple-b))))

(defun mapnumber (op tuple-a number)
  (list (funcall op (tuple-x tuple-a) number)
        (funcall op (tuple-y tuple-a) number)
        (funcall op (tuple-z tuple-a) number)
        (funcall op (tuple-w tuple-a) number)))

(defun mapcolor (op a b)
  (list (funcall op (red a) (red b))
        (funcall op (green a) (green b))
        (funcall op (blue a) (blue b))))

(defun tuple-to-list (tuple)
  (list (tuple-x tuple) (tuple-y tuple) (tuple-z tuple) (tuple-w tuple)))

(defmacro transform-object (&rest transforms)
  (declare (optimize (speed 3) (safety 0)))
  `(reduce #'mult (list ,@(reverse transforms))))

(defun reflect (in normal)
  (declare (tuple in) (tuple normal)
           (optimize (speed 3) (safety 0)))
  (sub in (mult normal (mult 2.0 (dot in normal)))))
