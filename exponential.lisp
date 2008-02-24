(in-package :randist)

(in-package :randist)

(declaim (optimize (speed 3) (debug 0) (safety 1)))

;;    The exponential distribution has the form

;;    p(x) dx = exp(-x/mu) dx/mu

;;    for x = 0 ... +infty 

;; double
;; gsl_ran_exponential (const gsl_rng * r, const double mu)
;; {
;;   double u = gsl_rng_uniform_pos (r);

;;   return -mu * log (u);
;; }

(defun random-exponential (mu)
  "Random values for:
p(x) dx = exp(-x/mu) dx/mu"
  (declare (type double-float mu))
  (* (- mu) (log (random-uniform))))

