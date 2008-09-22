(in-package :randist)

(declaim (optimize (speed 3) (debug 0) (safety 3)))

(declaim (inline transfer-sign))

(defun transfer-sign (a b)
  (if (>= b 0)
      (abs a)
      (- (abs a))))

(defun zeroin (ax bx f &optional (tol double-float-epsilon))
  "zero of the function  f(x)  is computed in the interval ax,bx .

  input..

  ax     left endpoint of initial interval
  bx     right endpoint of initial interval
  f      function subprogram which evaluates f(x) for any x in
         the interval  ax,bx
  tol    desired length of the interval of uncertainty of the
         final result ( .ge. 0.0d0)


  output..

  zeroin abcissa approximating a zero of  f  in the interval ax,bx


      it is assumed  that   f(ax)   and   f(bx)   have  opposite  signs
  without  a  check.  zeroin  returns a zero  x  in the given interval
  ax,bx  to within a tolerance  4*macheps*abs(x) + tol, where macheps
  is the relative machine precision.
      this function subprogram is a slightly  modified  translation  of
  the algol 60 procedure  zero  given in  richard brent, algorithms for
  minimization without derivatives, prentice - hall, inc. (1973)."

  (declare (type double-float ax bx)
	   (ftype (function (double-float) double-float) f))

  (let* ((a ax)
	 (b bx)
	 (fa (funcall f a))
	 (fb (funcall f b))
	 (eps double-float-epsilon)
	 (c) (fc) (d) (e) (tol1) (xm) (s) (p) (q) (r))
    (tagbody
     start
       (setf c a fc fa d (- b a) e d)
     swap
       (when (>= (abs fc) (abs fb))
	 (go convergence-test))
       ;; circle a,c <- b; b <-c 
       (setf a b b c c a fa fb fb fc fc fa)
     convergence-test
       (setf tol1 (+ (* 2.0 eps (abs b)) (* 0.5 tol)))
       (setf xm (* 0.5 (- c b)))
       (when (or (<= (abs xm) tol1) (= fb 0d0))
	 (go end))
       (when (or (< (abs e) tol1)
		 (<= (abs fa) (abs fb)))
	 (go bisection))
       (unless (= a c)
	 (go quadratic-interpolation))
       
       (setf s (/ fb fa)
	     p (* 2d0 xm s)
	     q (- 1d0 s))
       (go adjust-signs)
     quadratic-interpolation
       (setf q (/ fa fc)
	     r (/ fb fc)
	     s (/ fb fa)
	     p (* s (- (* 2d0 xm q (- q r)) (* (- b a) (- r 1d0))))
	     q (* (- q 1d0) (- r 1d0) (- s 1d0)))
     adjust-signs
       (when (>= p 0d0)
	 (setf q (- q)))
       (setf p (abs p))

       ;; is interpolation acceptable
       (when (or (>= (* 2d0 p) (- (* 3d0 xm q (abs (* tol1 q)))))
		 (>= p (abs (* 0.5d0 e q))))
	 (go bisection))
       (go complete)
     bisection
       (setf d xm e d)
     complete
       (setf a b fa fb)
       (if (> (abs d) tol1)
	 (setf b (+ b d))
	 (setf b (+ b (transfer-sign tol1 xm))))
       (setf fb (funcall f b))
       (when (> (* fb (/ fc (abs fc))) 0d0)
	 (go start))
       (go swap)
     end)
    b))
       
;;;;;;;;;;;;;;;;;;;
;;;
;;; RANDOM GIG
;;;
;;;;;;;;;;;;;;;;;;;	       


(defun gig-setup (lambda chi psi)
  (declare (type double-float lambda chi psi))
  (let* ((alpha (sqrt (/ psi chi)))
	(beta (sqrt (* psi chi)))
	(l1 (- lambda 1d0))
	(m (/ (+ l1 (sqrt (+ (* l1 l1) (* beta beta)))) beta)))

    #+TODO(print (list 'alpha alpha 'beta beta 'l1 l1 'm m))

    (flet ((g (y)
	     (+ (* 0.5d0 beta (expt y 3))
		(- (* y y (+ (* 0.5d0 beta m) lambda 1d0)))
		(* y (- (* l1 m) (* 0.5d0 beta)))
		(* 0.5d0 beta m))))

      #+null(print (list 'g (g m)))
 
      (let* ((upper (do ((x m (* 2 x)))
			((> (g x) 0d0) x)))		      
	     (yM (zeroin 0d0 m #'g))
	     (yP (zeroin m upper #'g))
	     (a (* (- yP m)
		   (expt (/ yP m) (* 0.5d0 l1))
		   (exp (* -0.25d0 beta (+ yP (/ yP) (- m) (- (/ m)))))))
	     (b (* (- yM m)
		   (expt (/ yM m) (* 0.5d0 l1))
		   (exp (* -0.25d0 beta (+ yM (/ yM) (- m) (- (/ m)))))))
	     (c (+ (* -0.25 beta (+ m (/ m))) (* 0.5d0 l1 (log m)))))
	(values l1 alpha beta m a b c)))))

	     
#+TODO(defun random-gig1 (chi psi)
  "Optimized version of random-gig for lambda=1"
  -42.0)

(declaim (inline %random-gig))  
(defun %random-gig (l1 alpha beta m a b c)
  (let ((Y))
    (tagbody
     start
       (let* ((R1 (random-uniform))
	      (R2 (random-uniform)))
	 (setf Y (+ m (* a (/ R2 R1)) (* b (/ (- 1d0 R2) R1))))
	 #+null(print (list 'R R1 R2))
	 #+null(print (list 'y y m a b))
	 (when (and (> Y 0)
		    (>= (- (log R1)) (+ (* -0.5d0 l1 (log Y)) 
					(* 0.25d0 beta (+ Y (/ Y))) c)))
	   #+null(print y)
	   (go end)))
       (go start)
     end)
    (/ Y alpha)))

(defun random-gig (lambda chi psi)
  "Random Generalized Inverse Poisson

The algorithm is based on that given by Dagpunar (1989)"
  (when (< chi 0) (error "chi can not be negative"))
  (when (< psi 0) (error "psi can not be negative"))

  #+null (when (= lambda 1)
	   (returnrn-from random-gig (random-gig1 chi psi)))
  
  (let* ((lambda (coerce lambda 'double-float))
	 (chi (coerce chi 'double-float))
	 (psi (coerce psi 'double-float)))

    (multiple-value-bind (l1 alpha beta m a b c) (gig-setup lambda chi psi)
      (%random-gig l1 alpha beta m a b c))))
	
	
(declaim (inline random-generalized-inverse-poisson))

(defun random-generalized-inverse-poisson (lambda chi psi)
  (random-GIG lambda chi psi))

  
(defun make-random-variable-gig (lambda chi psi)
  (when (< chi 0) (error "chi can not be negative"))
  (when (< psi 0) (error "psi can not be negative"))

  (let* ((lambda (coerce lambda 'double-float))
	 (chi (coerce chi 'double-float))
	 (psi (coerce psi 'double-float)))

    (multiple-value-bind (l1 alpha beta m a b c) (gig-setup lambda chi psi)
      (lambda ()
	(%random-gig l1 alpha beta m a b c)))))

(defun make-random-variable-gig-poisson (lambda chi psi)
  (let ((gig (make-random-variable-gig lambda chi psi)))
    (lambda ()
      (random-poisson (funcall gig)))))

(defun random-gig-iid (n lambda chi psi)
  "Random Generalized Inverse Poisson (vector version)"
  (let ((gig (make-random-variable-gig lambda chi psi)))
    (random-vector-iid n gig)))

;; Tests

(defun test-gig-range ()
  (let ((r (make-random-variable-gig-poisson -0.7277 0.0417 0.0016)))
    (time (loop 
	     for i from 0 to 100000
	     for x = (funcall r)  
	     counting (> x 0) into nz
	     minimizing x into min
	     maximizing x into max
	     finally (return (list min max nz))))))

(defun test-gig-speed ()
  (time (loop 
	   for i from 0 to 100000
	   for x = (random-poisson (random-gig  -0.7277 0.0417 0.0016))
	   counting (> x 0) into nz
	   minimizing x into min
	   maximizing x into max
	   finally (return (list min max nz)))))
  