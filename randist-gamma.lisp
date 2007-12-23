(in-package :randist)

;;; The Gamma distribution of order a>0 is defined by:
;;;
;;;   p(x) dx = {1 / \Gamma(a) b^a } x^{a-1} e^{-x/b} dx
;;;
;;;   for x>0.  If X and Y are independent gamma-distributed random
;;;   variables of order a1 and a2 with the same scale parameter b, then
;;;   X+Y has gamma distribution of order a1+a2.
;;;
;;;   The algorithms below are from Knuth, vol 2, 2nd ed, p. 129. 


;;;   Works only if a > 1, and is most efficient if a is large
;;;
;;;   This algorithm, reported in Knuth, is attributed to Ahrens.  A
;;;   faster one, we are told, can be found in: J. H. Ahrens and
;;;   U. Dieter, Computing 12 (1974) 223-246.  


(declaim (optimize (speed 3) (debug 2) (safety 2) (space 0) (compilation-speed 0)))

;; static double
;; gamma_large (const gsl_rng * r, const double a)
;; {
;;   double sqa, x, y, v;
;;   sqa = sqrt (2 * a - 1);
;;   do
;;     {
;;       do
;;         {
;;           y = tan (M_PI * gsl_rng_uniform (r));
;;           x = sqa * y + a - 1;
;;         }
;;       while (x <= 0);
;;       v = gsl_rng_uniform (r);
;;     }
;;   while (v > (1 + y * y) * exp ((a - 1) * log (x / (a - 1)) - sqa * y));
;;   return x;
;; }


(declaim (ftype (function (double-float) double-float) gamma-large)
	 (inline gamma-large))
(defun gamma-large (a)
  (declare (double-float a))
  
  (let* ((a-1 (- a 1d0))
	 (sqa (sqrt (- (* 2 a) 1)))
	 (x 0d0)
	 (y 0d0)
	 (v 0d0))

    (declare (double-float a-1 x y v sqa)
	     (dynamic-extent a-1 sqa y v))
    
    (tagbody
     start
       (setq y (tan (* pi (random-uniform))))
       (setq x (+ (* sqa  y) a-1))
       (when (<= x 0.0)
	 (go start))
       (setq v (random-uniform))
       
       (when (> v (* (+ (* y y) 1d0)
		     (exp (- (* a-1 (the double-float (log (/ x a-1))))
			     (* sqa y)))))
	 (go start)))
    x))



;; double gsl_ran_gamma_int (const gsl_rng * r, const unsigned int a)
;; {
;;   if (a < 12)
;;     {
;;       unsigned int i;
;;       double prod = 1;

;;       for (i = 0; i < a; i++)
;;         {
;;           prod *= gsl_rng_uniform_pos (r);
;;         }

;;       /* Note: for 12 iterations we are safe against underflow, since
;;          the smallest positive random number is O(2^-32). This means
;;          the smallest possible product is 2^(-12*32) = 10^-116 which
;;          is within the range of double precision. */

;;       return -log (prod);
;;     }
;;   else
;;     {
;;       return gamma_large (r, (double) a);
;;     }
;; }

(declaim (ftype (function (fixnum) double-float) gamma-int)
	 (inline gamma-int))

(defun gamma-int (a)
  (declare (fixnum a))
  (if (< a 12)
    (do ((i 0 (1+ i))
	 (prod 1d0 (* prod (random-uniform))))
	((= i a) (-  (log prod)))
      (declare (fixnum i)
	       (double-float prod)))
    (gamma-large (coerce a 'double-float))))



;; static double
;; gamma_frac (const gsl_rng * r, const double a)
;; {
;;   /* This is exercise 16 from Knuth; see page 135, and the solution is
;;      on page 551.  */

;;   double p, q, x, u, v;
;;   p = M_E / (a + M_E);
;;   do
;;     {
;;       u = gsl_rng_uniform (r);
;;       v = gsl_rng_uniform_pos (r);

;;       if (u < p)
;;         {
;;           x = exp ((1 / a) * log (v));
;;           q = exp (-x);
;;         }
;;       else
;;         {
;;           x = 1 - log (v);
;;           q = exp ((a - 1) * log (x));
;;         }
;;     }
;;   while (gsl_rng_uniform (r) >= q);

;;   return x;
;; }

(defconstant +e+ (exp 1d0))


(declaim (ftype (function () double-float) random-pos))
(declaim (inline random-pos))
(defun random-pos ()
  (let ((y 0d0))
    (tagbody
     start
       (setf y (random 1d0))
       (when (= y 0d0)
	 (go start)))
    y))
	
(declaim (ftype (function (double-float) double-float) gamma-frac)
	 (inline gamma-frac))
(defun gamma-frac (a)
  (declare (double-float a))
  (let	((p (/ +e+ (+ a +e+)))
	 (u 0d0)
	 (v 0d0)
	 (x 0d0)
	 (q 0d0))
    (declare (double-float p u v x q))
    (tagbody
     start
       (setf u (random-uniform))
       (setf v (random-pos))
       (if (< u p)
	   (progn
	     (setf x (exp (* (/ 1d0 a)  (log v))))
	     (setf q (exp (- x))))
	   (progn
	     (setf x (- 1d0 (log v)))
	     (setf q (exp (* (- a 1d0) (log x))))))
       (when (>= (random 1d0) q)
	 (go start)))
    x))



;; double
;; gsl_ran_gamma (const gsl_rng * r, const double a, const double b)
;; {
;;   /* assume a > 0 */
;;   unsigned int na = floor (a);

;;   if (a == na)
;;     {
;;       return b * gsl_ran_gamma_int (r, na);
;;     }
;;   else if (na == 0)
;;     {
;;       return b * gamma_frac (r, a);
;;     }
;;   else
;;     {
;;       return b * (gsl_ran_gamma_int (r, na) + gamma_frac (r, a - na)) ;
;;     }
;; }

(declaim (ftype (function (double-float double-float) double-float) random-gamma1)
	 (inline random-gamma1))

(defun random-gamma1 (a b)
  (declare (double-float a b))
  (assert (> a 0d0))
  (multiple-value-bind (na frac) (truncate a)
    (declare (dynamic-extent na frac))
    (if (= frac 0)
	(* b (gamma-int na))
	(if (= na 0)
	    (* b (gamma-frac a))
	    (* b (+ (gamma-int na) (gamma-frac frac)))))))
	 



;;  New version based on Marsaglia and Tsang, "A Simple Method for
;;  generating gamma variables", ACM Transactions on Mathematical
;;  Software, Vol 26, No 3 (2000), p363-372.

;;  Implemented by J.D.Lamb@btinternet.com, minor modifications for GSL
;;  by Brian Gough

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
	   (when (< (log u) (+ (* 0.5 x x) (* d (+ 1 (- v) (the double-float (log v))))))
	     (go end))
	   (go start)
	 end)
	(* b d v))))

	   
(declaim (inline random-gamma))

(defun random-gamma (a &optional (b 1d0))
  (random-gamma-mt a b))
