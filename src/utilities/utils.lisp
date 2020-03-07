(in-package #:lisp-tracer-utilities)

(defun inserter (insert-fun)
  (lambda (acc next)
    (if (listp next)
        (funcall insert-fun acc next)
        (list next acc))))

(defun insert-first (arg surround)
  (list* (car surround)
         arg
         (cdr surround)))

(defun insert-last (arg surround)
  (append surround (list arg)))

(defmacro -> (initial-form &rest forms)
  (reduce (inserter #'insert-first)
          forms
          :initial-value initial-form))

(defmacro ->> (initial-form &rest forms)
  (reduce (inserter #'insert-last)
          forms
          :initial-value initial-form))

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
