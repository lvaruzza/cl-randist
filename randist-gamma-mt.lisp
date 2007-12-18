(in-package :randist)

;;  New version based on Marsaglia and Tsang, "A Simple Method for
;;  generating gamma variables", ACM Transactions on Mathematical
;;  Software, Vol 26, No 3 (2000), p363-372.

;;  Implemented by J.D.Lamb@btinternet.com, minor modifications for GSL
;;  by Brian Gough


(declaim (optimize (speed 3) (debug 0) (safety 0) (space 0) (compilation-speed 0)))


;; double
;; gsl_ran_gamma_mt (const gsl_rng * r, const double a, const double b)
;; {
;;   /* assume a > 0 */

;;   if (a < 1)
;;     {
;;       double u = gsl_rng_uniform_pos (r);
;;       return gsl_ran_gamma_mt (r, 1.0 + a, b) * pow (u, 1.0 / a);
;;     }

;;   {
;;     double x, v, u;
;;     double d = a - 1.0 / 3.0;
;;     double c = (1.0 / 3.0) / sqrt (d);

;;     while (1)
;;       {
;;         do
;;           {
;;             x = gsl_ran_gaussian_ziggurat (r, 1.0);
;;             v = 1.0 + c * x;
;;           }
;;         while (v <= 0);

;;         v = v * v * v;
;;         u = gsl_rng_uniform_pos (r);

;;         if (u < 1 - 0.0331 * x * x * x * x) 
;;           break;

;;         if (log (u) < 0.5 * x * x + d * (1 - v + log (v)))
;;           break;
;;       }
    
;;     return b * d * v;
;;   }
;; }

(declaim (ftype (function (double-float double-float) double-float)
		random-gamma-mt))
(defun random-gamma-mt (a b)
  (declare (double-float a b))
  (if (< a 1d0)
      (* (random-gamma-mt (+ 1d0 a) b) (expt (random-uniform) (/ a)))
      (let* ((x 0d0)
	     (v 0d0)
	     (u 0d0)
	     (d (- a (/ 3d0)))
	     (c (/ (/ 3d0) (sqrt d))))

	(declare (double-float x v u d c))
	(tagbody
	 start
	   (setf x (random-normal-ziggurat 0d0 1d0))
	   (setf v (+ 1d0 (* c x)))
	   (when (<= v 0d0)
	     (go start))

	   (setf v (* v v v))
	   (setf u (random-uniform))
	   
	   (when (< u (- 1d0 (* 0.0331 x x x x)))
	     (go end))
	   (when (< (log u) (+ (* 0.5 x x) (* d (+ 1 (- v) (log v)))))
	     (go end))
	   (go start)
	 end)
	(* b d v))))

	   
		  