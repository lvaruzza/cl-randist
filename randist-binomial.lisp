(in-package :randist)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

;; /* The binomial distribution has the form,

;;    prob(k) =  n!/(k!(n-k)!) *  p^k (1-p)^(n-k) for k = 0, 1, ..., n

;;    This is the algorithm from Knuth */

;; /* Default binomial generator is now in binomial_tpe.c */

;; unsigned int
;; gsl_ran_binomial_knuth (const gsl_rng * r, double p, unsigned int n)
;; {
;;   unsigned int i, a, b, k = 0;

;;   while (n > 10)        /* This parameter is tunable */
;;     {
;;       double X;
;;       a = 1 + (n / 2);
;;       b = 1 + n - a;

;;       X = gsl_ran_beta (r, (double) a, (double) b);

;;       if (X >= p)
;;         {
;;           n = a - 1;
;;           p /= X;
;;         }
;;       else
;;         {
;;           k += a;
;;           n = b - 1;
;;           p = (p - X) / (1 - X);
;;         }
;;     }

;;   for (i = 0; i < n; i++)
;;     {
;;       double u = gsl_rng_uniform (r);
;;       if (u < p)
;;         k++;
;;     }

;;   return k;
;; }

(declaim (ftype (function (double-float fixnum) fixnum) random-binomial))

(defun random-binomial (p n)
  (let ((a 0) (b 0) (k 0)
	(X 0d0))

;;    (declaim (fixnum i a b k)
;;	     (double-float X))
    
    (tagbody
     start
       (setf a (1+ (floor n 2)))
       (setf b (1+ (- n a)))
       
       (setf X (random-beta (coerce a 'double-float)
			    (coerce b 'double-float)))

       (if (>= X p)
	   (progn
	     (setf n (1- a))
	     (setf p (/ p X)))
	   (progn
	     (incf k a)
	     (setf n (1- b))
	     (setf p (/ (- p X) (- 1d0 X)))))

       (when (> n 10)
	 (go start)))

    (loop
       for i from 0 to (1- n)
       for u = (random-uniform)
       when (< u p)
       do (incf k))
    k))


	     