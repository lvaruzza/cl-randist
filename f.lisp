(in-package :randist)

(declaim (optimize (speed 3) (debug 0) (safety 1)))

;; /* The F distribution has the form

;;    p(x) dx = (nu1^(nu1/2) nu2^(nu2/2) Gamma((nu1 + nu2)/2) /
;;    Gamma(nu1/2) Gamma(nu2/2)) *
;;    x^(nu1/2 - 1) (nu2 + nu1 * x)^(-nu1/2 -nu2/2) dx

;;    The method used here is the one described in Knuth */

;; double
;; gsl_ran_fdist (const gsl_rng * r, const double nu1, const double nu2)
;; {

;;   double Y1 =  gsl_ran_gamma (r, nu1 / 2, 2.0);
;;   double Y2 =  gsl_ran_gamma (r, nu2 / 2, 2.0);

;;   double f = (Y1 * nu2) / (Y2 * nu1);

;;   return f;
;; }

(defun random-f (nu1 nu2)
   "Random value for:

p(x) dx = (nu1^(nu1/2) nu2^(nu2/2) Gamma((nu1 + nu2)/2) /
Gamma(nu1/2) Gamma(nu2/2)) *
x^(nu1/2 - 1) (nu2 + nu1 * x)^(-nu1/2 -nu2/2) dx"
   
   (let ((y1 (random-gamma ((/ nu1 2d0) 2d0)))
	 (y2 (random-gamma ((/ nu2 2d0) 2d0))))

     (/ (* y1 nu2) (* y2 nu1))))
