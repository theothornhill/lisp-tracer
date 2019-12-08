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
