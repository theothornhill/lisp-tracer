(in-package #:lisp-tracer)

(defun projectile (position velocity)
  (list position velocity))

(defun environment (gravity wind)
  (list gravity wind))

(defun tick (env projectile)
  (print-object (first projectile) t)
  (print "")
  (let ((position (add (first projectile)
                       (second projectile)))
        (velocity (add (second projectile)
                       (add (first env)
                            (second env)))))
    (projectile position velocity)))

(defparameter *p* (projectile (point! 0 1 0) (normalize (vec! 1 1 0))))
(defparameter *e* (environment (vec! 0 -0.1 0) (vec! -0.01 0 0)))

(defun chapter01 ()
  (loop while (> (y (first *p*)) 0)
     do (setq *p* (tick *e* *p*))))
