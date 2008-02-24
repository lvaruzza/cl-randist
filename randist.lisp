(in-package :randist)

;; On CMUCL or SBCL the random already uses mersene twister
#+(or sbcl cmucl)
(progn
  (declaim (inline random-mt))
  (defun random-mt (x &optional (state *random-state*))
    (random x state)))

(defmacro random-uniform ()
  `(random-mt 1d0))

(declaim (ftype (function () double-float) random-pos))
(declaim (inline random-pos))
(defun random-pos ()
  (let ((y 0d0))
    (tagbody
     start
       (setf y (random-uniform))
       (when (= y 0d0)
	 (go start)))
    y))
