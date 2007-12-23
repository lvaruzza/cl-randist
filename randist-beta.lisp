(in-package :randist)

;; /* The beta distribution has the form
;;
;;    p(x) dx = (Gamma(a + b)/(Gamma(a) Gamma(b))) x^(a-1) (1-x)^(b-1) dx
;;
;;    The method used here is the one described in Knuth */
;;
;; double
;; gsl_ran_beta (const gsl_rng * r, const double a, const double b)
;; {
;;   double x1 = gsl_ran_gamma (r, a, 1.0);
;;   double x2 = gsl_ran_gamma (r, b, 1.0);

;;   return x1 / (x1 + x2);
;; }

(declaim (ftype (function (double-float double-float) double-float) random-beta))
(defun beta (a b)
  (declare (double-float a b))
  (let ((x1 (random-gamma a 1d0))
	(x2 (random-gamma b 1d0)))
    (declare (double-float x1 x2))
    (/ x1 (+ x1 x2))))
