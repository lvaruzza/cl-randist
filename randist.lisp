(in-package :randist)

;; On CMUCL or SBCL the random already uses mersene twister
;; ... why is jmt.lisp compiled in then? - PVK
#+(or sbcl cmucl)
(progn
  (declaim (inline random-mt))
  (defun random-mt (x &optional (state *random-state*))
    (random x state)))

;; either this is [0, 1), or it's broken. - PVK
(defmacro random-uniform ()
  "[syntax suggar] Random variable with uniform distribution in interval [0,1]"
  `(random-mt 1d0))

(declaim (ftype (function () (values (double-float (0d0) (1d0))
                                     &optional))
                random-pos)
         (inline random-pos))
(defun random-pos ()
  "Create the sign, i.e. random positive or negative, similar to a
binary result."
  (let ((y 0d0))
    (tagbody
     start
       (setf y (random-uniform))
       (when (= y 0d0)
	 (go start)))
    y))

(defun random-vector-iid (n variable)
  "Return a vector with n IID instances of variable"
  (let ((output (make-array n :element-type 'double-float :adjustable nil :fill-pointer nil)))
    (loop for i from 0 to (1- n)
       do (setf (aref output i) (funcall variable)))
    output))

(defun random-bernoulli (p)
  (if (> (random-uniform) p)
      1
      0))

