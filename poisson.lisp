(in-package :randist)

(declaim (optimize (speed 3) (debug 0) (safety 1)))

;; /* The poisson distribution has the form

;;    p(n) = (mu^n / n!) exp(-mu) 

;;    for n = 0, 1, 2, ... . The method used here is the one from Knuth. */

;; unsigned int
;; gsl_ran_poisson (const gsl_rng * r, double mu)
;; {
;;   double emu;
;;   double prod = 1.0;
;;   unsigned int k = 0;

;;   while (mu > 10)
;;     {
;;       unsigned int m = mu * (7.0 / 8.0);

;;       double X = gsl_ran_gamma_int (r, m);

;;       if (X >= mu)
;;         {
;;           return k + gsl_ran_binomial (r, mu / X, m - 1);
;;         }
;;       else
;;         {
;;           k += m;
;;           mu -= X; 
;;         }
;;     }

;;   /* This following method works well when mu is small */

;;   emu = exp (-mu);

;;   do
;;     {
;;       prod *= gsl_rng_uniform (r);
;;       k++;
;;     }
;;   while (prod > emu);

;;   return k - 1;

;; }


(declaim (ftype (function (double-float) integer) random-poisson))

(defun random-poisson (mu)
  (declare (type double-float mu))
  (let ((k 0))
    (declare (type integer k))
    (loop 
       for m integer = (truncate (* mu (/ 7d0 8d0)))
       for X double-float = (random-gamma-int m)
       while (> mu 10)
       do (if (>= X mu)
	      (return-from 
	       random-poisson
		(+ k (truncate
		      (random-binomial (/ mu X) (- m 1)))))
	      (progn
		(incf k m)
		(decf mu X))))

    (let ((prod 1d0)
	  (emu (exp (- mu))))
      (declare (type double-float prod emu))
      (loop 
	 do (progn
	      (setf prod (* prod (random-uniform)))
	      (incf k))
	 while (> prod emu)))
      
    (1- k)))

  