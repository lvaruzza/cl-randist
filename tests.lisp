(in-package :randist)

;;; Lists

(defun genlist (f n)
  (loop for i from 1 to n
       collect (funcall f i)))

(defun gen-uniform (&optional (n 100))
  (genlist #'(lambda (x) (declare (ignore x)) (random-uniform)) n))


;;(defun gen-uniform-mt (&optional (n 100) (max 1.0))
;;  (genlist #'(lambda (x) (declare (ignore x)) (mt19937:random max)) n))


;; (defun gen-normal (&optional (n 100))
;;  (genlist #'(lambda (x) (declare (ignore x)) (random-normal)) n))


(defun test-dist (f &optional (n 1000))
  (let ((lst (genlist f n)))
    (values (float (mean lst))
            (float (var lst)))))

(defvar *n* (* 100 1000 1000))
(defun test-gamma-speed (&optional (n *n*))
  (time (dotimes (i n)
          (random-gamma1 2.4d0 0.5d0))))


(defun test-gamma-mt-speed (&optional (n *n*))
  (time (dotimes (i n)
          (random-gamma-mt 2.4d0 0.5d0))))

(defun test-zigg-speed (&optional (n *n*))
  (time (dotimes (i n)
          (random-normal-ziggurat 0d0 1d0))))

(defun test-binomial (&optional (n *n*))
  (test-dist #'(lambda (x)
                 (declare (ignore x))
                 (random-binomial 0.3d0 10)) n))


(defun test-random-uniform-jmt (&optional (n *n*))
  (test-dist #'(lambda (x)
                 (declare (ignore x))
                 (random-uniform-jmt)) n))

#+sbcl
(defun profile-gamma (&optional (n (* 100 1000)))
  (sb-profile:reset)
  (sb-profile:profile random-gamma gamma-frac gamma-large gamma-int random)
  (test-gamma-speed n)
  (sb-profile:report)
  (sb-profile:unprofile random-gamma gamma-frac gamma-large gamma-int random))


