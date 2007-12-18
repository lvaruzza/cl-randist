(in-package :kempbasu)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Distributional functions
;;;

;;;
;;; Binomial and Poisson distributions
;;;

;; BIONOMIAL-PROBABILITY 
;; exact: Rosner 93, approximate 105

;; P(X=k) for X a binomial random variable with parameters n & p.
;; Binomial expectations for seeing k events in N trials, each having
;; probability p.  Use the Poisson approximation if N>100 and P<0.01. 

(defun binomial-probability (n k p)
  #+debug(test-variables (n :posint) (p :prob) 
                  ("K must be between 0 and N (inclusive)" :test (and (>= k 0) (<= k n))))
  (if (and (> n 100) (< p 0.01))
      (poisson-probability (* n p) k)
      (let ((p (coerce p 'double-float)))
        (* (choose n k)
           (expt p k)
           (expt (- 1 p) (- n k))))))

;; BINOMIAL-CUMULATIVE-PROBABILITY
;; Rosner 94

;; P(X<k) for X a binomial random variable with parameters n & p.
;; Bionomial expecations for fewer than k events in N trials, each having
;; probability p.

(defun binomial-cumulative-probability (n k p)
  #+debug(test-variables (n :posint) (k :posint) (p :prob)
             ("K must be less than or equal to N" :test (<= k n)))
  (let ((sum-up-to-k-1 0d0))
    (dotimes (i k sum-up-to-k-1)
      (incf sum-up-to-k-1 (binomial-probability n i p)))))

;; BINOMIAL-GE-PROBABILITY
;; Rosner 94

;; The probability of k or more occurances in N events, each with
;; probability p.

(defun binomial-ge-probability (n k p)
  (- 1 (binomial-cumulative-probability n k p)))

;; Just for convenience later, binomial-le-probability

(defun binomial-le-probability (n k p)
  #+debug(test-variables (n :posint) (k :posint) (p :prob)
             ("K must be less than or equal to N" :test (<= k n)))
  (let ((sum-up-to-k 0d0))
    (dotimes (i (1+ k) sum-up-to-k)
      (incf sum-up-to-k (binomial-probability n i p)))))


;; POISSON-PROBABILITY
;; Rosner 100

;; Probability of seeing k events over a time period when the expected
;; number of events over that time is mu.

(defun poisson-probability (mu k)
  #+debug(test-variables (mu :posnum) (k :posint))
  (let ((mu (coerce mu 'double-float)))
    (/ (* (exp (- mu)) (expt mu k))
       (factorial k))))

;; POISSON-CUMULATIVE-PROBABILITY
;; Rosner 197

;; Probability of seeing fewer than K events over a time period when the
;; expected number events over that time is mu.

(defun poisson-cumulative-probability (mu k)
  #+debug(test-variables (mu :posnum) (k :posint))
  (if (< k 170)
      (let ((sum 0d0))
        (dotimes (x k sum)
          (incf sum (poisson-probability mu x))))
      (let ((mu (coerce mu 'double-float))
            (k (coerce k 'double-float)))
        (- 1d0 (gamma-incomplete k mu)))))
  

;; POISSON-GE-PROBABILITY
;; Probability of X or more events when expected is mu.

(defun poisson-ge-probability (mu x)
  (- 1 (poisson-cumulative-probability mu x)))

;;;
;;;  Normal distributional functions
;;;

;; NORMAL-PDF
;; The probability density function (PDF) for a normal distribution with
;; mean mu and variance sigma at point x.

;; Rosner 115

(defun Normal-pdf (x mu sigma)
  #+debug(test-variables (x number) (mu number) (sigma :posnum))
  (* (/ (* (sqrt (* 2 pi)) sigma))
     (exp (* (- (/ (* 2 (square sigma)))) (square (- x mu))))))


;; CONVERT-TO-STANDARD-NORMAL
;; Rosner 130
;; Convert X from a Normal distribution with mean mu and variance sigma to
;; standard normal

(defun convert-to-standard-normal (x mu sigma)
  #+debug(test-variables (x number) (mu number) (sigma :posnum))
  (/ (- x mu) sigma))

;; PHI
;; the CDF of standard normal distribution
;; Rosner 125

(defun phi (x)
  "Adopted from CLASP 1.4.3, see copyright notice at http://eksl-www.cs.umass.edu/clasp.html"
  #+debug(test-variables (x number))
  (setf x (coerce x 'double-float))
  (locally (declare (type double-float x))
    (* 0.5d0 (+ 1.0d0 (error-function (/ x (sqrt 2.0d0)))))))

;; Z
;; The inverse normal function, P(X<Zu) = u where X is distributed as the
;; standard normal.  Uses binary search.
;; Rosner 128. 

(defun z (percentile &key (epsilon 1d-15))
  #+debug(test-variables (percentile :prob))
  (let ((target (coerce percentile 'double-float)))
    (do ((min -9d0 min)
         (max 9d0 max)
         (guess 0d0 (+ min (/ (- max min) 2d0))))
        ((< (- max min) epsilon) guess)
      (let ((result (coerce (phi guess) 'double-float)))
        (if (< result target)
            (setq min guess)
            (setq max guess))))))
            
;; T-DISTRIBUTION
;; Rosner 178
;; Returns the point which is the indicated percentile in the T distribution
;; with dof degrees of freedom

(defun t-distribution (dof percentile)
  "Adopted from CLASP 1.4.3, http://eksl-www.cs.umass.edu/clasp.html"
  #+debug(test-variables (dof :posint) (percentile :prob))
  (find-critical-value
   #'(lambda (x) (t-significance x dof :tails :positive))
   (- 1 percentile)))


;; CHI-SQUARE
;; Rosner 187
;; Returns the point which is the indicated percentile in the Chi Square
;; distribution with dof degrees of freedom.

(defun chi-square (dof percentile)
  #+debug(test-variables (dof :posint) (percentile :prob))
  (find-critical-value #'(lambda (x) (chi-square-cdf x dof))
                       (- 1 percentile)))

;; Chi-square-cdf computes the left hand tail area under the chi square
;; distribution under dof degrees of freedom up to X. 

(defun chi-square-cdf (x dof)
  "Adopted from CLASP 1.4.3, http://eksl-www.cs.umass.edu/clasp.html"
  #+debug(test-variables (x :posnum) (dof :posint))
  (multiple-value-bind (cdf ignore)
      (gamma-incomplete (* 0.5 dof) (* 0.5 x))
    (declare (ignore ignore))
    cdf))
   
