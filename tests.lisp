(in-package :kempbasu)

(defun gen-uniform (&optional (n 100) (max 1.0))
  (genlist #'(lambda (x) (declare (ignore x)) (random max)) n))


;;(defun gen-uniform-mt (&optional (n 100) (max 1.0))
;;  (genlist #'(lambda (x) (declare (ignore x)) (mt19937:random max)) n))


(defun gen-normal (&optional (n 100))
  (genlist #'(lambda (x) (declare (ignore x)) (random-normal)) n))


(defun gen-gamma-large (&optional (n 100) (a 20))
  (let ((a  (coerce a 'double-float)))
    (genlist #'(lambda (x) (declare (ignore x)) (gamma-large a) n))))

(defun gen-gamma-int (&optional (n 100) (a 5))
  (genlist #'(lambda (x) (declare (ignore x)) (gamma-int a)) n))

(defun gen-gamma-frac (&optional (n 100) (a 0.5d0))
  (genlist #'(lambda (x) (declare (ignore x)) (gamma-frac a)) n))


(defun gen-gamma (&optional (n 100) (a 5.5d0) (b 2d0))
  (genlist #'(lambda (x) (declare (ignore x)) (random-gamma a b)) n))


(defun gen-zigg (&optional (n 100) (m 0d0) (sigma 1d0))
  (genlist #'(lambda (x) (declare (ignore x)) (random-normal-ziggurat m sigma)) n))

(defun test-gamma-speed (n)
  (time (dotimes (i n)
	  (random-gamma 2.4d0 0.5d0))))
  

(defun test-zigg-speed (n)
  (time (dotimes (i n)
	  (random-gamma 2.4d0 0.5d0))))

(defun profile-gamma (&optional (n (* 100 1000)))
  (sb-profile:reset)
  (sb-profile:profile random-gamma gamma-frac gamma-large gamma-int random)
  (test-gamma-speed n)
  (sb-profile:report)
  (sb-profile:unprofile random-gamma gamma-frac gamma-large gamma-int random))


