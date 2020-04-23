(in-package #:lisp-tracer-tests)

(deftest ray-sphere-intersect
  (testing "A ray intersects a sphere at two points"
    (let* ((r (make-ray :origin (make-point :x 0.0 :y 0.0 :z -5.0)
                        :direction (make-vec :x 0.0 :y 0.0 :z 1.0)))
           (s (make-sphere))
           (xs (intersect s r)))
      (ok (equal? (length xs) 2))
      (ok (equal? (rt-intersection-tt (car xs)) 4.0))
      (ok (equal? (rt-intersection-tt (cadr xs)) 6.0)))))

(deftest ray-intersect-tangent
  (testing "A ray intersect a sphere at a tangent"
    (let* ((r (make-ray :origin (make-point :x 0.0 :y 1.0 :z -5.0)
                        :direction (make-vec :x 0.0 :y 0.0 :z 1.0)))
           (s (make-sphere))
           (xs (intersect s r)))
      (ok (equal? (length xs) 2))
      (ok (equal? (rt-intersection-tt (car xs)) 5.0))
      (ok (equal? (rt-intersection-tt (cadr xs)) 5.0)))))

(deftest ray-intersect-miss
  (testing "A ray misses a sphere"
    (let* ((r (make-ray :origin (make-point :x 0.0 :y 2.0 :z -5.0)
                        :direction (make-vec :x 0.0 :y 0.0 :z 1.0)))
           (s (make-sphere))
           (xs (intersect s r)))
      (ok (equal? (length xs) 0)))))

(deftest ray-intersect-inside
  (testing "A ray originates inside a sphere"
    (let* ((r (make-ray :origin (make-point)
                        :direction (make-vec :x 0.0 :y 0.0 :z 1.0)))
           (s (make-sphere))
           (xs (intersect s r)))
      (ok (equal? (length xs) 2))
      (ok (equal? (rt-intersection-tt (car xs)) -1.0))
      (ok (equal? (rt-intersection-tt (cadr xs)) 1.0)))))

(deftest ray-intersect-outside
  (testing "A sphere is behind a ray"
    (let* ((r (make-ray :origin (make-point :x 0.0 :y 0.0 :z 5.0)
                        :direction (make-vec :x 0.0 :y 0.0 :z 1.0)))
           (s (make-sphere))
           (xs (intersect s r)))
      (ok (equal? (length xs) 2))
      (ok (equal? (rt-intersection-tt (car xs)) -6.0))
      (ok (equal? (rt-intersection-tt (cadr xs)) -4.0)))))

(deftest intersect-set-object
  (testing "Intersect sets the object on the intersection"
    (let* ((r (make-ray :origin (make-point :x 0.0 :y 0.0 :z -5.0)
                        :direction (make-vec :x 0.0 :y 0.0 :z 1.0)))
           (s (make-sphere))
           (xs (intersect s r)))
      (ok (equal? (length xs) 2))
      (ok (eq (rt-intersection-object (first xs)) s))
      (ok (eq (rt-intersection-object (second xs)) s)))))

(deftest sphere-transformations
  (testing "The default transformation"
    (let ((s (make-shape)))
      (ok (equal? (shape-transform s) (identity-matrix)))))
  (testing "Assigning a transformation"
    (let ((s (make-shape))
          (trans (translation 2.0 3.0 4.0)))
      (setf (shape-transform s) trans)
      (ok (equal? (shape-transform s) trans)))))

(deftest intersecting-spheres
  (testing "Intersecting a scaled sphere with a ray"
    (let* ((r (make-ray :origin (make-point :x 0.0 :y 0.0 :z -5.0)
                        :direction (make-vec :x 0.0 :y 0.0 :z 1.0)))
           (s (make-sphere)))
      (setf (shape-transform s) (scaling 2.0 2.0 2.0))
      (let ((xs (intersect s r)))
        (ok (= (length xs) 2))
        (ok (= (rt-intersection-tt (car xs)) 3.0))
        (ok (= (rt-intersection-tt (cadr xs)) 7.0)))))
  (testing "Intersecting a translated sphere with a ray"
    (let* ((r (make-ray :origin (make-point :x 0.0 :y 0.0 :z -5.0)
                        :direction (make-vec :x 0.0 :y 0.0 :z 1.0)))
           (s (make-sphere)))
      (setf (shape-transform s) (translation 5.0 0.0 0.0))
      (let ((xs (intersect s r)))
        (ok (= (length xs) 0))))))

(deftest normal-testing
  (testing "The normal on a sphere at a point on the x axis"
    (let* ((s (make-sphere))
           (n (normal-at s (make-point :x 1.0 :y 0.0 :z 0.0))))
      (ok (equal? n (make-vec :x 1.0 :y 0.0 :z 0.0)))))
  (testing "The normal on a sphere at a point on the y axis"
    (let* ((s (make-sphere))
           (n (normal-at s (make-point :x 0.0 :y 1.0 :z 0.0))))
      (ok (equal? n (make-vec :x 0.0 :y 1.0 :z 0.0)))))
  (testing "The normal on a sphere at a point on the z axis"
    (let* ((s (make-sphere))
           (n (normal-at s (make-point :x 0.0 :y 0.0 :z 1.0))))
      (ok (equal? n (make-vec :x 0.0 :y 0.0 :z 1.0)))))
  (testing "The normal on a sphere at a nonaxial point"
    (let* ((s (make-sphere))
           (sqrt3/3 (/ (sqrt 3) 3.0))
           (n (normal-at s (make-point :x sqrt3/3 :y sqrt3/3 :z sqrt3/3))))
      (ok (equal? n (make-vec :x sqrt3/3 :y sqrt3/3 :z sqrt3/3)))))
  (testing "The normal is a normalized vector"
    (let* ((s (make-sphere))
           (sqrt3/3 (/ (sqrt 3) 3.0))
           (n (normal-at s (make-point :x sqrt3/3 :y sqrt3/3 :z sqrt3/3))))
      (ok (equal? n (normalize n))))))

(deftest normalizing-transformed-sphere
  (testing "Computing the normal on a translated sphere"
    (let* ((s (make-sphere)))
      (setf (shape-transform s) (translation 0.0 1.0 0.0))
      (let ((n (normal-at s (make-point :x 0.0 :y 1.70711 :z -0.70711))))
        (ok (equal? n (make-vec :x 0.0 :y 0.70711 :z -0.70711))))))
  (testing "Computing the normal on a transformed sphere"
    (let* ((s (make-sphere))
           (sqrt2/2 (/ (sqrt 2) 2.0))
           (m (transform-object
               (rotation-z (div (coerce pi 'double-float) 5.0))
               (scaling 1.0 0.5 1.0))))
      (setf (shape-transform s) m)
      (let ((n (normal-at s (make-point :x 0.0 :y sqrt2/2 :z (neg sqrt2/2)))))
        (ok (equal? n (make-vec :x 0.0 :y 0.97014 :z -0.24254)))))))

(deftest sphere-materials
  (testing "The default material"
    (let ((s (make-shape)))
      (ok (equal? (shape-material s) (make-material)))))
  (testing "Assigning a material"
    (let ((s (make-shape))
          (m (make-material)))
      (setf (material-ambient m) 1.0)
      (setf (shape-material s) m)
      (ok (equal? (shape-material s) m))))
  (testing "A helper for producing a sphere with a glassy material"
    (let ((s (glass-sphere)))
      (ok (equal? (shape-transform s) (identity-matrix)))
      (ok (equal? (material-transparency (shape-material s)) 1.0))
      (ok (equal? (material-refractive-index (shape-material s)) 1.5)))))

(deftest cube-testing
  (testing "A ray intersects a cube"
    (let ((points (list
                   (make-point :x 5.0 :y 0.5 :z 0.0)
                   (make-point :x -5.0 :y 0.5 :z 0.0)
                   (make-point :x 0.5 :y 5.0 :z 0.0)
                   (make-point :x 0.5 :y -5.0 :z 0.0)
                   (make-point :x 0.5 :y 0.0 :z 5.0)
                   (make-point :x 0.5 :y 0.0 :z -5.0)
                   (make-point :x 0.0 :y 0.5 :z 0.0)))
          (vecs (list
                 (make-vec :x -1.0 :y 0.0 :z 0.0)
                 (make-vec :x 1.0 :y 0.0 :z 0.0)
                 (make-vec :x 0.0 :y -1.0 :z 0.0)
                 (make-vec :x 0.0 :y 1.0 :z 0.0)
                 (make-vec :x 0.0 :y 0.0 :z -1.0)
                 (make-vec :x 0.0 :y 0.0 :z 1.0)
                 (make-vec :x 0.0 :y 0.0 :z 1.0)))
          (t1s (list 4 4 4 4 4 4 -1))
          (t2s (list 6 6 6 6 6 6 1))
          (c (make-cube)))
      (mapcar (lambda (p v t1 t2)
                (let* ((r (make-ray :origin p :direction v))
                       (xs (local-intersect c r)))
                  (ok (= (length xs) 2))
                  (ok (= (rt-intersection-tt (car xs)) t1))
                  (ok (= (rt-intersection-tt (cadr xs)) t2))))
              points vecs t1s t2s)))
  (testing "A ray misses a cube"
    (let ((points (list
                   (make-point :x -2.0 :y 0.0 :z 0.0)
                   (make-point :x 0.0 :y -2.0 :z 0.0)
                   (make-point :x 0.0 :y 0.0 :z -2.0)
                   (make-point :x 2.0 :y 0.0 :z 2.0)
                   (make-point :x 0.0 :y 2.0 :z 2.0)
                   (make-point :x 2.0 :y 2.0 :z 0.0)))
          (vecs (list
                 (make-vec :x 0.2673 :y 0.5345 :z 0.8018)
                 (make-vec :x 0.8018 :y 0.2673 :z 0.5345)
                 (make-vec :x 0.5345 :y 0.8018 :z 0.2673)
                 (make-vec :x 0.0 :y 0.0 :z -1.0)
                 (make-vec :x 0.0 :y -1.0 :z 0.0)
                 (make-vec :x -1.0 :y 0.0 :z 0.0)))
          (c (make-cube)))
      (mapcar (lambda (p v)
                (let* ((r (make-ray :origin p :direction v))
                       (xs (local-intersect c r)))
                  (ok (= (length xs) 0))))
              points vecs)))
  (testing "The normal on the surface of a cube"
    (let ((points (list
                   (make-point :x 1.0 :y 0.5 :z -0.8)
                   (make-point :x -1.0 :y -0.2 :z 0.9)
                   (make-point :x -0.4 :y 1.0 :z -0.1)
                   (make-point :x 0.3 :y -1.0 :z -0.7)
                   (make-point :x -0.6 :y 0.3 :z 1.0)
                   (make-point :x 0.4 :y 0.4 :z -1.0)
                   (make-point :x 1.0 :y 1.0 :z 1.0)
                   (make-point :x -1.0 :y -1.0 :z -1.0)))
          (normals (list
                    (make-vec :x 1.0 :y 0.0 :z 0.0)
                    (make-vec :x -1.0 :y 0.0 :z 0.0)
                    (make-vec :x 0.0 :y 1.0 :z 0.0)
                    (make-vec :x 0.0 :y -1.0 :z 0.0)
                    (make-vec :x 0.0 :y 0.0 :z 1.0)
                    (make-vec :x 0.0 :y 0.0 :z -1.0)
                    (make-vec :x 1.0 :y 0.0 :z 0.0)
                    (make-vec :x -1.0 :y 0.0 :z 0.0)))
          (c (make-cube)))
      (mapcar (lambda (p n)
                (let* ((normal (local-normal-at c p)))
                  (ok (equal? normal n))))
              points normals))))

(deftest cylinder-testing
  (testing "A ray misses a cylinder"
    (let ((origins (list
                    (make-point :x 1.0 :y 0.0 :z 0.0)
                    (make-point :x 0.0 :y 0.0 :z 0.0)
                    (make-point :x 0.0 :y 0.0 :z -5.0)))
          (directions (list
                       (make-vec :x 0.0 :y 1.0 :z 0.0)
                       (make-vec :x 0.0 :y 1.0 :z 0.0)
                       (make-vec :x 1.0 :y 1.0 :z 1.0)))
          (c (make-cylinder)))
      (mapcar (lambda (o d)
                (let* ((direction (normalize d))
                       (r (make-ray :origin o :direction direction))
                       (xs (local-intersect c r)))
                  (ok (= (length xs) 0))))
              origins directions)))
  (testing "A ray intersects a cylinder"
    (let ((origins (list
                    (make-point :x 1.0 :y 0.0 :z -5.0)
                    (make-point :x 0.0 :y 0.0 :z -5.0)
                    (make-point :x 0.5 :y 0.0 :z -5.0)))
          (directions (list
                       (make-vec :x 0.0 :y 0.0 :z 1.0)
                       (make-vec :x 0.0 :y 0.0 :z 1.0)
                       (make-vec :x 0.1 :y 1.0 :z 1.0)))
          (t0s (list 5.0 4.0 6.80798191702732))
          (t1s (list 5.0 6.0 7.088723439378861))
          (c (make-cylinder)))
      (mapcar (lambda (o d t0 t1)
                (let* ((direction (normalize d))
                       (r (make-ray :origin o :direction direction))
                       (xs (local-intersect c r)))
                  (ok (= (length xs) 2))
                  (ok (= (rt-intersection-tt (car xs)) t0))
                  (ok (= (rt-intersection-tt (cadr xs)) t1))))
              origins directions t0s t1s)))
  (testing "The normal on the surface of a cylinder"
    (let ((points (list
                   (make-point :x 1.0 :y 0.0 :z 0.0)
                   (make-point :x 0.0 :y 5.0 :z -1.0)
                   (make-point :x 0.0 :y 2.0 :z 1.0)
                   (make-point :x -1.0 :y 1.0 :z 0.0)))
          (normals (list
                    (make-vec :x 1.0 :y 0.0 :z 0.0)
                    (make-vec :x 0.0 :y 0.0 :z -1.0)
                    (make-vec :x 0.0 :y 0.0 :z 1.0)
                    (make-vec :x -1.0 :y 0.0 :z 0.0)))
          (c (make-cylinder)))
      (mapcar (lambda (p n) (ok (equal? (local-normal-at c p) n)))
              points normals)))
  (testing "The default minimum and maximum for a cylinder"
    (let ((c (make-cylinder)))
      (ok (= (cylinder-minimum c) sb-ext:double-float-negative-infinity))
      (ok (= (cylinder-maximum c) sb-ext:double-float-positive-infinity))))
  (testing "Intersecting a constrained cylinder"
    (let ((origins (list
                    (make-point :x 0.0 :y 1.5 :z 0.0)
                    (make-point :x 0.0 :y 3.0 :z -5.0)
                    (make-point :x 0.0 :y 0.0 :z -5.0)
                    (make-point :x 0.0 :y 2.0 :z -5.0)
                    (make-point :x 0.0 :y 1.0 :z -5.0)
                    (make-point :x 0.0 :y 1.5 :z -2.0)))
          (directions (list
                       (make-vec :x 0.1 :y 1.0 :z 0.0)
                       (make-vec :x 0.0 :y 0.0 :z 1.0)
                       (make-vec :x 0.0 :y 0.0 :z 1.0)
                       (make-vec :x 0.0 :y 0.0 :z 1.0)
                       (make-vec :x 0.0 :y 0.0 :z 1.0)
                       (make-vec :x 0.0 :y 0.0 :z 1.0)))
          (counts (list 0 0 0 0 0 2))
          (c (make-cylinder :minimum 1.0 :maximum 2.0)))
      (mapcar (lambda (o d count)
                (let* ((direction (normalize d))
                       (r (make-ray :origin o :direction direction))
                       (xs (local-intersect c r)))
                  (ok (= (length xs) count))))
              origins directions counts)))
  (testing "The default closed value for a cylinder"
    (let ((c (make-cylinder)))
      (ng (cylinder-closed c))))
  (testing "Intersecting the caps of a closed cylinder"
    (let ((origins (list
                    (make-point :x 0.0 :y 3.0 :z 0.0)
                    (make-point :x 0.0 :y 3.0 :z -2.0)
                    (make-point :x 0.0 :y 4.0 :z -2.0)
                    (make-point :x 0.0 :y 0.0 :z -2.0)
                    (make-point :x 0.0 :y -1.0 :z -2.0)))
          (directions (list
                       (make-vec :x 0.0 :y -1.0 :z 0.0)
                       (make-vec :x 0.0 :y -1.0 :z 2.0)
                       (make-vec :x 0.0 :y -1.0 :z 1.0)
                       (make-vec :x 0.0 :y 1.0 :z 2.0)
                       (make-vec :x 0.0 :y 1.0 :z 1.0)))
          (c (make-cylinder :minimum 1.0 :maximum 2.0 :closed t)))
      (mapcar (lambda (o d)
                (let* ((direction (normalize d))
                       (r (make-ray :origin o :direction direction))
                       (xs (local-intersect c r)))
                  (ok (= (length xs) 2))))
              origins directions)))
  (testing "The normal vector on a cylinder's end caps"
    (let ((points (list
                   (make-point :x 0.0 :y 1.0 :z 0.0)
                   (make-point :x 0.5 :y 1.0 :z 0.0)
                   (make-point :x 0.0 :y 1.0 :z 0.5)
                   (make-point :x 0.0 :y 2.0 :z 0.0)
                   (make-point :x 0.5 :y 2.0 :z 0.0)
                   (make-point :x 0.0 :y 2.0 :z 0.5)))
          (normals (list
                    (make-vec :x 0.0 :y -1.0 :z 0.0)
                    (make-vec :x 0.0 :y -1.0 :z 0.0)
                    (make-vec :x 0.0 :y -1.0 :z 0.0)
                    (make-vec :x 0.0 :y 1.0 :z 0.0)
                    (make-vec :x 0.0 :y 1.0 :z 0.0)
                    (make-vec :x 0.0 :y 1.0 :z 0.0)))
          (c (make-cylinder :minimum 1.0 :maximum 2.0 :closed t)))
      (mapcar (lambda (p n) (ok (equal? (local-normal-at c p) n)))
              points normals))))

(deftest cone-testing
  (testing "A ray intersects a cone"
    (let ((origins (list
                    (make-point :x 0.0 :y 0.0 :z -5.0)
                    (make-point :x 0.0 :y 0.0 :z -5.0)
                    (make-point :x 1.0 :y 1.0 :z -5.0)))
          (directions (list
                       (make-vec :x 0.0 :y 0.0 :z 1.0)
                       (make-vec :x 1.0 :y 1.0 :z 1.0)
                       (make-vec :x -0.5 :y -1.0 :z 1.0)))
          (t0s (list 5.0 8.660254037844386 4.550055679356349))
          (t1s (list 5.0 8.660254037844386 49.449944320643645))
          (c (make-cone)))
      (mapcar (lambda (o d t0 t1)
                (let* ((direction (normalize d))
                       (r (make-ray :origin o :direction direction))
                       (xs (local-intersect c r)))
                  (ok (= (length xs) 2))
                  (ok (= (rt-intersection-tt (car xs)) t0))
                  (ok (= (rt-intersection-tt (cadr xs)) t1))))
              origins directions t0s t1s)))
  (testing "Intersecting a cone with a ray parallel to one of its halves"
    (let* ((c (make-cone))
           (direction (normalize (make-vec :x 0.0 :y 1.0 :z 1.0)))
           (r (make-ray :origin (make-point :x 0.0 :y 0.0 :z -1.0)
                        :direction direction))
           (xs (local-intersect c r)))
      (ok (= (length xs) 1))
      (ok (= (rt-intersection-tt (car xs)) 0.3535533905932738))))
  (testing "Intersecting a cone's end caps"
    (let ((origins (list
                    (make-point :x 0.0 :y 0.0 :z -5.0)
                    (make-point :x 0.0 :y 0.0 :z -0.25)
                    (make-point :x 0.0 :y 0.0 :z -0.25)))
          (directions (list
                       (make-vec :x 0.0 :y 1.0 :z 0.0)
                       (make-vec :x 0.0 :y 1.0 :z 1.0)
                       (make-vec :x 0.0 :y 1.0 :z 0.0)))
          (counts (list 0 2 4))
          (c (make-cone :minimum -0.5 :maximum 0.5 :closed t)))
      (mapcar (lambda (o d count)
                (let* ((direction (normalize d))
                       (r (make-ray :origin o :direction direction))
                       (xs (local-intersect c r)))
                  (ok (= (length xs) count))))
              origins directions counts)))
  (testing "The normal vector on a cone"
    (let ((points (list
                   (make-point :x 0.0 :y 0.0 :z 0.0)
                   (make-point :x 1.0 :y 1.0 :z 1.0)
                   (make-point :x -1.0 :y -1.0 :z 0.0)))
          (normals (list
                    (make-vec :x 0.0 :y 0.0 :z 0.0)
                    (make-vec :x 1.0 :y (coerce (- (sqrt 2)) 'double-float) :z 1.0)
                    (make-vec :x -1.0 :y 1.0 :z 0.0)))
          (c (make-cone)))
      (mapcar (lambda (p n) (ok (equal? (local-normal-at c p) n)))
              points normals))))
