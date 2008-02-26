(in-package :randist)

(declaim (optimize (speed 3) (safety 1)))

;;    The negative binomial distribution has the form,
;;    prob(k) =  Gamma(n + k)/(Gamma(n) Gamma(k + 1))  p^n (1-p)^k 
;;    for k = 0, 1, ... . Note that n does not have to be an integer.
;;    This is the Leger's algorithm (given in the answers in Knuth) 

;; unsigned int
;; gsl_ran_negative_binomial (const gsl_rng * r, double p, double n)
;; {
;;   double X = gsl_ran_gamma (r, n, 1.0) ;
;;   unsigned int k = gsl_ran_poisson (r, X*(1-p)/p) ;
;;   return k ;
;; }


(declaim (ftype (function (double-float integer) integer) random-negative-binomial)
	 (inline random-negative-binomial))

(defun random-negative-binomial (p n)
  (declare (type double-float p)
	   (integer n))
  (let ((X (random-gamma (coerce n 'double-float) 1d0)))
    (random-poisson (* X (/ (- 1d0 p) p)))))

