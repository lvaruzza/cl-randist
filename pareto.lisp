(in-package :randist)

(declaim (optimize (speed 3) (debug 0) (safety 1)))

;; /* The Pareto distribution has the form,

;;    p(x) dx = (a/b) / (x/b)^(a+1) dx     for x >= b

;;  */

;; double
;; gsl_ran_pareto (const gsl_rng * r, double a, const double b)
;; {
;;   double x = gsl_rng_uniform_pos (r);

;;   double z = pow (x, -1 / a);

;;   return b * z;
;; }


(declaim (ftype (function (double-float double-float) double-float) random-pareto)
	 (inline random-pareto))

(defun random-pareto (a b)
  "Random value for parato distribution:
p(x) dx = (a/b) / (x/b)^(a+1) dx     for x >= b"

  (declare (type double-float a b))
  (let* ((x (random-pos))
	 (z (expt x (/ -1d0 a))))
    (* b z)))
