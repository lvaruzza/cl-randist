(in-package :randist)

(declaim (inline random-uniform))
(defun random-uniform ()
  "[syntax suggar] Random variable with uniform distribution in interval [0,1)"
  #+(or sbcl cmucl)
  (random 1d0)
  #+(not (or sbcl cmucl))
  (random-mt 1d0))

(declaim (inline random-uniform-jmt))
(defun random-uniform-jmt ()
  "[syntax suggar] Random variable with uniform distribution in interval [0,1)"
  (random-mt 1d0))

(declaim (ftype (function () (values (double-float (0d0) (1d0))
                                     &optional))
                random-pos)
         (inline random-pos))
(defun random-pos ()
  "Create the sign, i.e. random positive or negative, similar to a
binary result."
  (loop
   (let ((y (random-uniform)))
     (when (> y 0d0)
       (return y)))))

(defun random-vector-iid (n variable)
  "Return a vector with n IID instances of variable"
  (map-into (make-array n :element-type 'double-float
                          :adjustable nil
                          :fill-pointer nil)
            variable))

(defun random-bernoulli (p)
  (if (> (random-uniform) p)
      1
      0))

