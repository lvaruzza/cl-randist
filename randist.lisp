(in-package :randist)

(defmacro random-uniform ()
  `(random 1d0))

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
