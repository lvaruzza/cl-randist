(in-package :randist)

(declaim (optimize (speed 3) (debug 0) (safety 1)))

;; /* The chisq distribution has the form

;;    p(x) dx = (1/(2*Gamma(nu/2))) (x/2)^(nu/2 - 1) exp(-x/2) dx

;;    for x = 0 ... +infty */

;; double
;; gsl_ran_chisq (const gsl_rng * r, const double nu)
;; {
;;   double chisq = 2 * gsl_ran_gamma (r, nu / 2, 1.0);
;;   return chisq;
;; }


(declaim (ftype (function (double-float) double-float) random-chi-square)
	 (inline random-chi-square))

(defun random-chi-square (nu)
  "Generate random variable for chi square distribution:

p(x) dx = (1/(2*Gamma(nu/2))) (x/2)^(nu/2 - 1) exp(-x/2) dx"
  
  (declare (type double-float nu))
  (* 2d0 (random-gamma (/ nu 2d0))))
