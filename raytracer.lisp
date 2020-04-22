(defun sq(x) (* x x))

(defun mag (x y z)
  (sqrt (+ (sq x) (sq y) (sq z))))

(defun unit-vector (x y z)
  (let ((d (mag x y z)))
    (values (/ x d) (/ y d) (/ z d))))

(defstruct (point (:conc-name nil))
  x y z)

(defun distance (p1 p2)
  (mag (- (x p1) (x p2))
       (- (y p1) (y p2))
       (- (z p1) (z p2))
       ))

(defun minroot(a b c)
  (if (zerop a)
    (/ (- c) b)
    (let ((disc (- (sq b) (* 4 a c))))
      (unless (minusp disc)
        (let ((discrt (sqrt disc)))
          (min (/ (+ (- b) discrt) (* 2 a))
               (/ (- (- b) discrt) (* 2 a))))))))

(defstruct surface color)
(defparameter *world* nil)
(defconstant eye (make-point :x 0 :y 0 :z 200))

(defun tracer (pathname &optional (res 1))
  (with-open-file (p pathname :direction :output)
    (format p "P2 ~A ~A 255" (* res 100) (* res 100))
    (let((inc (/ res)))
         (do ((y -50 (+ y inc)))
           ((< (- 50 y) inc))
           (do ((x -50 (+ x inc)))
             ((< (- 50  x) inc))
             (print (color-at x y) p))))))

(defun color-at (x y)
  (multiple-value-bind (xr yr zr)
                       (unit-vector (- x (x eye))
                                    (- y (y eye))
                                    (- 0 (z eye)))
                       (round (* (sendray eye xr yr zr) 255))))

(defun sendray (pt xr yr zr)
  (multiple-value-bind (s int) (first-hit pt xr yr zr)
                       (if s
                         (* (lambert s int xr yr zr) (surface-color s))
                         0)))

(defun first-hit (pt xr yr zr)
  (let(surface hit dist)
    (dolist (s *world*)
            (let ((h (intersect s pt xr yr zr)))
              (when h
                (let ((d (distance h pt)))
                  (when (or (null dist) (<d dist))
                    (setf surface s hit h dist d))))))
    (values surface hit)))

(defun lambert (s int xr yr zr)
  (multiple-value-bind (xn yn zn) (normal s int)
                       (max 0 (+ (* xr xn) (* yr yn) (* zr zn)))))
