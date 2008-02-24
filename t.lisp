(in-package :randist)

(declaim (optimize (speed 1) (debug 3) (safety 1)))

;; /* The t-distribution has the form

;;    p(x) dx = (Gamma((nu + 1)/2)/(sqrt(pi nu) Gamma(nu/2))
;;    * (1 + (x^2)/nu)^-((nu + 1)/2) dx

;;    The method used here is the one described in Knuth */

;; double
;; gsl_ran_tdist (const gsl_rng * r, const double nu)
;; {
;;   if (nu <= 2)
;;     {
;;       double Y1 = gsl_ran_ugaussian (r);
;;       double Y2 = gsl_ran_chisq (r, nu);

;;       double t = Y1 / sqrt (Y2 / nu);

;;       return t;
;;     }
;;   else
;;     {
;;       double Y1, Y2, Z, t;
;;       do
;;         {
;;           Y1 = gsl_ran_ugaussian (r);
;;           Y2 = gsl_ran_exponential (r, 1 / (nu/2 - 1));

;;           Z = Y1 * Y1 / (nu - 2);
;;         }
;;       while (1 - Z < 0 || exp (-Y2 - Z) > (1 - Z));

;;       /* Note that there is a typo in Knuth's formula, the line below
;;          is taken from the original paper of Marsaglia, Mathematics of
;;          Computation, 34 (1980), p 234-256 */

;;       t = Y1 / sqrt ((1 - 2 / nu) * (1 - Z));
;;       return t;
;;     }
;; }


(declaim (ftype (function (double-float) double-float) random-t random-t<=2 random-t>2)
	 (inline random-t<=2 random-t>2))

(defun random-t-nu<=2 (nu)
  (declare (type double-float nu))
  (let ((y1 (random-normal))
	(y2 (random-chi-square nu)))
    (/ y1 (sqrt (/ y2 nu)))))

(defun random-t-nu>2 (nu)
  (declare (type double-float nu))
  (let ((y1 0d0) (y2 0d0) (Z 0d0))
    (tagbody
     start
       (setf y1 (random-normal))
       (setf y2 (random-exponential (/ (- (/ nu 2d0) 1d0))))
       (setf z (/ (* y1 y1) (- nu 2d0)))
       (when (or (< (- 1d0 z) 0)
		 (> (exp (- (- y2) z)) (- 1d0 z)))
	 (go start)))
    
    (/ y1 (sqrt (* (- 1d0 (/ 2d0 nu)) (- 1d0 z))))))
    
(defun random-t (nu)
  (declare (type double-float nu))

  (if (<= nu 2d0)
      (random-t-nu<=2 nu)
      (random-t-nu>2 nu)))

	