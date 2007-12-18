;;; <code>

;;; Statistical functions in Common Lisp.  Version 1.04 Feb 24, 2005
;;;
;;; This code is copyright (c) 2000, 2001, 2002, 2005 by Larry Hunter
;;; (Larry.Hunter@uchsc.edu) except where otherwise noted.

;;; Thanks to Paul Cohen for the original CLASP package.  Thanks also to bug
;;; reports from Rob St. Amant and Lee Ayres, and several bug fixes from
;;; Robert Goldman. 
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by the
;;; Free Software Foundation; either version 2 of the License, or (at your
;;; option) any later version.

;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public License along
;;; with this program; if not, write to the Free Software Foundation, Inc.,
;;; 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;;; The formulas and methods used are largely taken from Bernard Rosner,
;;; "Fundamentals of Biostatistics," 5th edition.  "Rosner x" is a page
;;; number.  Some numeric functions were taken from CLASP, a 1994 common
;;; lisp package that implemented some of the statistical functions from
;;; "Numeric recipes in C" For CLASP functions, see copyright notice below.

;;;  These abreviations used in function and variable names:
;;;    ci = confidence interval
;;;    cdf = cumulative density function
;;;    ge = greater than or equal to
;;;    le = less than or equal to
;;;    pdf = probability density function
;;;    sd = standard deviation
;;;    rxc = rows by columns
;;;    sse = sample size estimate


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions provided:
;;;
;;;  Descriptive statistics
;;;   Mean, median, mode, geometric mean, range, percentile, variance,
;;;   standard-deviation (sd), coefficient-of-variation,
;;;   standard-error-of-the-mean
;;;
;;;  Distributional functions
;;;   Poisson & Binomial
;;;    binomial-probability, binomial-cumulative-probability,
;;;    binomial-ge-probability, poisson-probability,
;;;    poisson-cumulative-probability, poisson-ge-probability, Normal
;;;    normal-pdf, convert-to-standard-normal, phi, z, t-distribution,
;;;    chi-square, chi-square-cdf
;;;
;;;  Confidence Intervals

;;;   binomial-probability-ci, poisson-mu-ci, normal-mean-ci,
;;;   normal-mean-ci-on-sequences, normal-variance-ci,
;;;   normal-variance-ci-on-sequence, normal-sd-ci
;;;
;;;  Hypothesis tests (parametric)
;;;   z-test, z-test-on-sequence, t-test-one-sample,
;;;   t-test-one-sample-on-sequence, t-test-paired,
;;;   t-test-paired-on-sequences, t-test-two-sample,
;;;   t-test-two-sample-on-sequences, chi-square-test-one-sample, f-test,
;;;   binomial-test-one-sample, binomial-test-two-sample, fisher-exact-test,
;;;   mcnemars-test, poisson-test-one-sample
;;;
;;;  Hypothesis tests (non-parametric)
;;;   sign-test, sign-test-on-sequence, wilcoxon-signed-rank-test,
;;;   chi-square-test-rxc, chi-square-test-for-trend

;;;  Sample size estimates
;;;   t-test-one-sample-sse, t-test-two-sample-sse
;;;   t-test-paired-sse, binomial-test-one-sample-sse,
;;;   binomial-test-two-sample-sse,
;;;   binomial-test-paired-sse, correlation-sse

;;;  Correlation and Regression
;;;   linear-regression, correlation-coefficient,
;;;   correlation-test-two-sample, spearman-rank-correlation

;;;  Significance test functions
;;;   t-significance, f-significance (chi square significance is calculated
;;;   from chi-square-cdf in various ways depending on the problem).

;;;  Utilities
;;;   random-sample, random-pick, bin-and-count, fishers-z-transform,
;;;   mean-sd-n, square, choose, permutations, round-float


(declaim (optimize (speed 3) (safety 2) (debug 1)))

(in-package :statistics)

;;;;; Macros

;; This macro makes assertions more readable.  There are several special
;; types defined: :probability (:prob), :positive-integer (:posint),
;; :positive-number (:posnum), :number-sequence (:numseq),
;; :positive-integer-sequence (:posintseq), :probability-sequence
;; (:probseq), :nonzero-number-sequence (:nonzero-numseq) and :percentage
;; (:percent).  Other assertions are assumed to be internal types.  The
;; arguments to test-variables are lists.  The first element of the list is
;; a variable name, and the second element is either a special or built-in
;; type.  If the variable binding is not of the type specified, and error is
;; signalled indicating the problem.  One variable may have multiple type
;; requirements, which are conjunctive.

(eval-when (:compile-toplevel :load-toplevel :execute)

(defmacro test-variables (&rest args)
  (let ((assertions nil))
    (dolist (arg args (append `(or ,@(nreverse assertions))))
      (let* ((name (first arg))
             (type (second arg))
             (test (case type
                     ((:probability :prob)
                      `(and (numberp ,name) (not (minusp ,name)) (<= ,name 1)))
                     ((:positive-integer :posint)
                      `(and (integerp ,name) (plusp ,name)))
                     ((:positive-number :posnum)
                      `(and (numberp ,name) (plusp ,name)))
                     ((:number-sequence :numseq)
                      `(and (typep ,name 'sequence) (every #'numberp ,name)
                            (not (null ,name))))
                     ((:nonzero-number-sequence :nonzero-numseq)
                      `(and (typep ,name 'sequence) (not (null ,name))
                        (every #'(lambda (x) (and (numberp x) (not (= 0 x))))
                         ,name)))
                     ((:probability-sequence :probseq)
                      `(and (typep ,name 'sequence) (not (null ,name))
                        (every #'(lambda (x) (and (numberp x) (not (minusp x))
                                                  (<= x 1.0))) ,name)))
                     ((:positive-integer-sequence :posintseq)
                      `(and (typep ,name 'sequence) (not (null ,name))
                        (every #'(lambda (x) (and (typep x 'integer) (plusp
                                                                      x)))
                         ,name)))
                     (:percentage
                      `(and (numberp ,name) (plusp ,name) (<= ,name 100)))
                     (:test (third arg))
                     (t `(typep ,name ',type))))
             (message `(error
                        ,(if (eql type :test)
                             name
                             (format nil "~a = ~~a is not a ~a" name
                                     (case type
                                       ((:positive-integer :posint)
                                        "positive integer")
                                       ((:positive-number :posnum)
                                        "positive number")
                                       ((:probability :prob) "probability")
                                       ((:number-sequence :numseq)
                                        "sequence of numbers")
                                       ((:nonzero-number-sequence
                                         :nonzero-numseq)
                                        "sequence of non-zero numbers")
                                       ((:positive-integer-sequence :posintseq)
                                        "sequence of positive integers")
                                       ((:probability-sequence :probseq)
                                        "sequence of probabilities")
                                       ((:percent :percentile) "percent")
                                       (t type))))
                        ,name)))
        (push `(unless ,test ,message) assertions)))))

;; SQUARE

(defmacro square (x)
  `(* ,x ,x))


(defmacro underflow-goes-to-zero (&body body)
  "Protects against floating point underflow errors and sets the value to 0.0 instead."
  `(handler-case 
       (progn ,@body)
     (floating-point-underflow (condition)
       (declare (ignore condition))
       (values 0.0d0))))


) ;end eval-when

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Descriptive statistics
;;;

;; MEAN 
;; Rosner 10 

(defun mean (sequence)
  (test-variables (sequence :numseq))
  (/ (reduce #'+ sequence) (length sequence)))

;; MEDIAN
;; Rosner 12 (and 19)

(defun median (sequence)
  (test-variables (sequence :numseq))
  (percentile sequence 50))

;; MODE
;; Rosner 14
;; returns two values: a list of the modes and the number of times they
;; occur.   Rob St. Amant <stamant@csc.ncsu.edu> suggested using a hash
;; table instead of an alist, and Lee Ayres <ayres@acm.org> noted that 
;; my revision failed to handle multiple modes properly.

(defun mode (sequence)
  (test-variables (sequence :numseq))
  (let ((count-table (make-hash-table :test #'eql))
	(modes nil)
	(mode-count 0))
    (map nil (lambda (elt) (incf (gethash elt count-table 0))) sequence)
    (maphash (lambda (key value)
               (cond ((> value mode-count)
		      (setf modes (list key)
			    mode-count value))
		     ((= value mode-count)
		      (push key modes))))
             count-table)
    (values modes mode-count)))


;; GEOMETRIC-MEAN
;; Rosner 16

(defun geometric-mean (sequence &optional (base 10))
  (test-variables (sequence :nonzero-numseq) (base :posnum))
  (expt base (mean (map 'list #'(lambda (x) (log x base)) sequence))))

;; RANGE
;; Rosner 18

(defun range (sequence)
  (test-variables (sequence :numseq))
  (- (reduce #'max sequence) (reduce #'min sequence)))

;; PERCENTILE
;; Rosner 19
;; NB: Aref is 0 based!

(defun percentile (sequence percent)
  (test-variables (sequence :numseq) (percent :percentage))
  (let* ((sorted-vect (coerce (sort (copy-seq sequence) #'<) 'simple-vector))
         (n (length sorted-vect))
         (k (* n (/ percent 100)))
         (floor-k (floor k)))
    (if (= k floor-k)
        (/ (+ (aref sorted-vect k)
              (aref sorted-vect (1- k)))
           2)
        (aref sorted-vect floor-k))))
      
;; VARIANCE
;; Rosner 21

(defun variance (sequence)
  (test-variables (sequence :numseq))
  (let ((mean (mean sequence))
        (n (length sequence)))
    (/ (reduce #'+ (map 'list #'(lambda (x) (square (- mean x))) sequence))
       (1- n))))

;; STANDARD-DEVIATION (SD)
;; Rosner 21

(defun standard-deviation (sequence)
  (test-variables (sequence :numseq))
  (let ((mean (mean sequence))
        (n (length sequence)))
    (sqrt (/ (reduce #'+ (map 'list #'(lambda (x) (square (- mean x))) sequence))
             (1- n)))))


(defun sd (sequence)
  (test-variables (sequence :numseq))
  (let ((mean (mean sequence))
        (n (length sequence)))
    (sqrt (/ (reduce #'+ (map 'list #'(lambda (x) (square (- mean x))) sequence))
             (1- n)))))


;; COEFFICIENT-OF-VARIATION
;; Rosner 24

(defun coefficient-of-variation (sequence)
  (* 100 (/ (standard-deviation sequence) (mean sequence))))

;; STANDARD-ERROR-OF-THE-MEAN
;; Rosner 172

(defun standard-error-of-the-mean (sequence)
  (/ (standard-deviation sequence) (sqrt (length sequence))))

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
  (test-variables (n :posint) (p :prob) 
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
  (test-variables (n :posint) (k :posint) (p :prob)
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
  (test-variables (n :posint) (k :posint) (p :prob)
             ("K must be less than or equal to N" :test (<= k n)))
  (let ((sum-up-to-k 0d0))
    (dotimes (i (1+ k) sum-up-to-k)
      (incf sum-up-to-k (binomial-probability n i p)))))


;; POISSON-PROBABILITY
;; Rosner 100

;; Probability of seeing k events over a time period when the expected
;; number of events over that time is mu.

(defun poisson-probability (mu k)
  (test-variables (mu :posnum) (k :posint))
  (let ((mu (coerce mu 'double-float)))
    (/ (* (exp (- mu)) (expt mu k))
       (factorial k))))

;; POISSON-CUMULATIVE-PROBABILITY
;; Rosner 197

;; Probability of seeing fewer than K events over a time period when the
;; expected number events over that time is mu.

(defun poisson-cumulative-probability (mu k)
  (test-variables (mu :posnum) (k :posint))
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
  (test-variables (x number) (mu number) (sigma :posnum))
  (* (/ (* (sqrt (* 2 pi)) sigma))
     (exp (* (- (/ (* 2 (square sigma)))) (square (- x mu))))))


;; CONVERT-TO-STANDARD-NORMAL
;; Rosner 130
;; Convert X from a Normal distribution with mean mu and variance sigma to
;; standard normal

(defun convert-to-standard-normal (x mu sigma)
  (test-variables (x number) (mu number) (sigma :posnum))
  (/ (- x mu) sigma))

;; PHI
;; the CDF of standard normal distribution
;; Rosner 125

(defun phi (x)
  "Adopted from CLASP 1.4.3, see copyright notice at http://eksl-www.cs.umass.edu/clasp.html"
  (test-variables (x number))
  (setf x (coerce x 'double-float))
  (locally (declare (type double-float x))
    (* 0.5d0 (+ 1.0d0 (error-function (/ x (sqrt 2.0d0)))))))

;; Z
;; The inverse normal function, P(X<Zu) = u where X is distributed as the
;; standard normal.  Uses binary search.
;; Rosner 128. 

(defun z (percentile &key (epsilon 1d-15))
  (test-variables (percentile :prob))
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
  (test-variables (dof :posint) (percentile :prob))
  (find-critical-value
   #'(lambda (x) (t-significance x dof :tails :positive))
   (- 1 percentile)))


;; CHI-SQUARE
;; Rosner 187
;; Returns the point which is the indicated percentile in the Chi Square
;; distribution with dof degrees of freedom.

(defun chi-square (dof percentile)
  (test-variables (dof :posint) (percentile :prob))
  (find-critical-value #'(lambda (x) (chi-square-cdf x dof))
                       (- 1 percentile)))

;; Chi-square-cdf computes the left hand tail area under the chi square
;; distribution under dof degrees of freedom up to X. 

(defun chi-square-cdf (x dof)
  "Adopted from CLASP 1.4.3, http://eksl-www.cs.umass.edu/clasp.html"
  (test-variables (x :posnum) (dof :posint))
  (multiple-value-bind (cdf ignore)
      (gamma-incomplete (* 0.5 dof) (* 0.5 x))
    (declare (ignore ignore))
    cdf))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Confidence intervals
;;;

;; BINOMIAL-PROBABILITY-CI
;; Rosner 193 (approximate) & 194 (exact)

;; Confidence intervals on a binomial probability.  If a binomial
;; probability of p has been observed in N trials, what is the 1-alpha
;; confidence interval around p?  Approximate (using normal theory
;; approximation) when npq >= 10 unless told otherwise

(defun binomial-probability-ci (n p alpha &key exact?)
  (test-variables (n :posint) (p :prob) (alpha :prob))
  (if (and (> (* n p (- 1 p)) 10) (not exact?))
      (let ((difference (* (z (- 1 (/ alpha 2)))
                           (sqrt (/ (* p (- 1 p)) n)))))
        (values (- p difference) (+ p difference)))
      (values (find-critical-value
               (lambda (p1) (binomial-cumulative-probability n (floor (* p n)) p1))
               (- 1 (/ alpha 2)))
              (find-critical-value
               (lambda (p2) (binomial-cumulative-probability n (1+ (floor (* p n))) p2))
               (/ alpha 2)))))

;; POISSON-MU-CI
;; Confidence interval for the Poisson parameter mu
;; Rosner 197
;;
;; Given x observations in a unit of time, what is the 1-alpha confidence
;; interval on the Poisson parameter mu (= lambda*T)?
;;
;; Since find-critical-value assumes that the function is monotonic
;; increasing, adjust the value we are looking for taking advantage of
;; reflectiveness

(defun poisson-mu-ci (x alpha)
  (test-variables (x :posnum) (alpha :prob))
  (values
   (find-critical-value
    #'(lambda (mu) (poisson-cumulative-probability mu (1- x)))
    (- 1 (/ alpha 2)))
   (find-critical-value
    #'(lambda (mu) (poisson-cumulative-probability mu x))
    (/ alpha 2))))
                 

;; NORMAL-MEAN-CI
;; Confidence interval for the mean of a normal distribution
;; Rosner 180

;; The 1-alpha percent confidence interval on the mean of a normal
;; distribution with parameters mean, sd & n. 

(defun normal-mean-ci (mean sd n alpha)
  (test-variables (mean number) (sd :posnum) (n :posint) (alpha :prob))
  (let ((t-value (t-distribution (1- n) (- 1 (/ alpha 2)))))
    (values (- mean (* t-value (/ sd (sqrt n))))
            (+ mean (* t-value (/ sd (sqrt n)))))))

;; NORMAL-MEAN-CI-ON-SEQUENCE
;;
;; The 1-alpha confidence interval on the mean of a sequence of numbers
;; drawn from a Normal distribution.

(defun normal-mean-ci-on-sequence (sequence alpha)
  (test-variables (alpha :prob)) ; sequence tested by mean-sd-n
  (multiple-value-call #'normal-mean-ci (mean-sd-n sequence) alpha)) 

;; NORMAL-VARIANCE-CI
;; Rosner 190
;; The 1-alpha confidence interval on the variance of a sequence of numbers
;; drawn from a Normal distribution.

(defun normal-variance-ci (variance n alpha)
  (test-variables (variance :posnum) (n :posint) (alpha :prob))
  (let ((chi-square-low (chi-square (1- n) (- 1 (/ alpha 2))))
        (chi-square-high (chi-square (1- n) (/ alpha 2)))
        (numerator (* (1- n) variance)))
    (values (/ numerator chi-square-low) (/ numerator chi-square-high))))

;; NORMAL-VARIANCE-CI-ON-SEQUENCE

(defun normal-variance-ci-on-sequence (sequence alpha)
  (test-variables (sequence :numseq) (alpha :prob))
  (let ((variance (variance sequence))
        (n (length sequence)))
    (normal-variance-ci variance n alpha)))

;; NORMAL-SD-CI
;; Rosner 190
;; As above, but a confidence inverval for the standard deviation.

(defun normal-sd-ci (sd n alpha)
  (multiple-value-bind (low high)
      (normal-variance-ci (square sd) n alpha)
    (values (sqrt low) (sqrt high))))

;; NORMAL-SD-CI-ON-SEQUENCE

(defun normal-sd-ci-on-sequence (sequence alpha)
  (test-variables (sequence :numseq) (alpha :prob))
  (let ((sd (standard-deviation sequence))
        (n (length sequence)))
    (normal-sd-ci sd n alpha)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Hypothesis testing
;;;

;; Z-TEST
;; Rosner 228

;; The significance of a one sample Z test for the mean of a normal
;; distribution with known variance.  mu is the null hypothesis mean, x-bar
;; is the observed mean, sigma is the standard deviation and N is the number
;; of observations.  If tails is :both, the significance of a difference
;; between x-bar and mu.  If tails is :positive, the significance of x-bar
;; is greater than mu, and if tails is :negative, the significance of x-bar
;; being less than mu.  Returns a p value.

(defun z-test (x-bar n &key (mu 0) (sigma 1) (tails :both))
  (test-variables (x-bar number) (n :posint) (mu number) (sigma :posnum))
  (let ((z (/ (- x-bar mu) (/ sigma (sqrt n)))))
    (ecase tails
      (:both (if (< z 0)
                 (* 2 (phi z))
                 (* 2 (- 1 (phi z)))))
      (:negative (phi z))
      (:positive (- 1 (phi z))))))

;; Z-TEST-ON-SEQUENCE

(defun z-test-on-sequence (sequence &key (mu 0) (sigma 1) (tails :both))
  (test-variables (sequence :numseq)) ; the rest handled by z-test
  (let ((x-bar (mean sequence))
        (n (length sequence)))
    (z-test x-bar n :mu mu :sigma sigma :tails tails)))

;; T-TEST-ONE-SAMPLE
;; T-TEST-ONE-SAMPLE-ON-SEQUENCE
;; Rosner 216

;; The significance of a one sample T test for the mean of a normal
;; distribution with unknown variance.  X-bar is the observed mean, sd is
;; the observed standard deviation, N is the number of observations and mu
;; is the test mean.  -ON-SAMPLE is the same, but calculates the observed
;; values from a sequence of numbers.

(defun t-test-one-sample (x-bar sd n mu &key (tails :both))
  (test-variables (x-bar number) (sd :posnum) (n :posint) (mu number))
  (t-significance  (/ (- x-bar mu) (/ sd (sqrt n))) (1- n) :tails tails))

(defun t-test-one-sample-on-sequence (sequence mu &key (tails :both))
  (multiple-value-call #'t-test-one-sample
    (mean-sd-n sequence) mu :tails tails))

;; T-TEST-PAIRED
;; Rosner 276

;; The significance of a paired t test for the means of two normal
;; distributions in a longitudinal study.  D-bar is the mean difference, sd
;; is the standard deviation of the differences, N is the number of pairs. 

(defun t-test-paired (d-bar sd n &key (tails :both))
  (test-variables (d-bar number) (sd :posnum) (n :posint))
  (t-significance (/ d-bar (/ sd (sqrt n))) (1- n) :tails tails))

;; T-TEST-PAIRED-ON-SEQUENCES
;; Rosner 276

;; The significance of a paired t test for means of two normal distributions
;; in a longitudinal study.  Before is a sequence of before values, after is
;; the sequence of paired after values (which must be the same length as the
;; before sequence).

(defun t-test-paired-on-sequences (before after &key (tails :both))
  (test-variables (before :numseq) (after :numseq)
             ("Before and after sequences must be of equal length"
              :test (= (length before) (length after))))
  (multiple-value-call #'t-test-paired
     (mean-sd-n (map 'list #'- before after)) :tails tails))

;; T-TEST-TWO-SAMPLE
;; Rosner  282, 294, 297 

;; The significance of the difference of two means (x-bar1 and x-bar2) with
;; standard deviations sd1 and sd2, and sample sizes n1 and n2 respectively.
;; The form of the two sample t test depends on whether the sample variances
;; are equal or not.   If the variable variances-equal? is :test, then we
;; use an F test and the variance-significance-cutoff to determine if they
;; are equal.  If the variances are equal, then we use the two sample t test
;; for equal variances.  If they are not equal, we use the Satterthwaite
;; method, which has good type I error properties (at the loss of some
;; power).  

(defun t-test-two-sample (x-bar1 sd1 n1 x-bar2 sd2 n2 &key
                          (variances-equal? :test)
                          (variance-significance-cutoff 0.05)
                          (tails :both))
  (test-variables (x-bar1 number) (sd1 :posnum) (n1 :posint)
             (x-bar2 number) (sd2 :posnum) (n2 :posint))
  (let (t-statistic dof)
    (if (ecase variances-equal?
          (:test (> (f-test (square sd1) n1 (square sd2) n2 :tails tails)
                    variance-significance-cutoff))
          ((t :yes :equal) t)
          ((nil :no :unequal) nil))
        (let ((s (sqrt (/ (+ (* (1- n1) (square sd1))
                             (* (1- n2) (square sd2)))
                          (+ n1 n2 -2)))))
          (setq t-statistic  (/ (- x-bar1 x-bar2)
                                (* s (sqrt (+ (/ n1) (/ n2)))))
                dof (+ n1 n2 -2)))
        (let* ((variance-ratio1 (/ (square sd1) n1))
               (variance-ratio2 (/ (square sd2) n2)))
          (setq t-statistic (/ (- x-bar1 x-bar2)
                               (sqrt (+ variance-ratio1 variance-ratio2)))
                dof (round (/ (square (+ variance-ratio1 variance-ratio2))
                              (+ (/ (square variance-ratio1) (1- n1))
                                 (/ (square variance-ratio2) (1- n2))))))))
    (t-significance t-statistic dof :tails tails)))


  
;; T-TEST-TWO-SAMPLE-ON-SEQUENCES
;; Same as above, but providing the sequences rather than the summaries.

(defun t-test-two-sample-on-sequences (sequence1 sequence2 &key
                                       (variance-significance-cutoff 0.05)
                                       (tails :both))
  (multiple-value-call #'t-test-two-sample
    (mean-sd-n sequence1) (mean-sd-n sequence2) :tails tails
    :variance-significance-cutoff variance-significance-cutoff))


;; F-TEST
;; Rosner 290
;; F test for the equality of two variances

(defun f-test (variance1 n1 variance2 n2 &key (tails :both))
  (test-variables (variance1 :posnum) (n1 :posint) (variance2 :posnum) (n2 :posint))
  (let ((significance (f-significance (/ variance1 variance2) (1- n1) (1- n2)
                                      (not (eql tails :both)))))
    (ecase tails
      (:both significance)
      (:positive (if (> variance1 variance2) significance (- 1 significance)))
      (:negative (if (< variance1 variance2) significance (- 1 significance))))))


;; CHI-SQUARE-TEST-ONE-SAMPLE
;; Rosner 246
;; The significance of a one sample Chi square test for the variance of a
;; normal distribution.  Variance is the observed variance, N is the number
;; of observations, and sigma-squared is the test variance.

(defun chi-square-test-one-sample (variance n sigma-squared &key (tails :both))
  (test-variables (variance :posnum) (n :posint) (sigma-squared :posnum))
  (let ((cdf (chi-square-cdf (/ (* (1- n) variance) sigma-squared) (1- n))))
    (ecase tails
      (:negative cdf)
      (:positive (- 1 cdf))
      (:both (if (<= variance sigma-squared) 
                 (* 2 cdf)
                 (* 2 (- 1 cdf)))))))


;; BINOMIAL-TEST-ONE-SAMPLE
;; Rosner 249
;; The significance of a one sample test for the equality of an observed
;; probability p-hat to an expected probability p under a binomial
;; distribution with N observations.  Use the normal theory approximation if
;; n*p*(1-p) > 10 (unless the exact flag is true). 

(defun binomial-test-one-sample (p-hat n p &key (tails :both) (exact? nil))
  (test-variables (p-hat :prob) (n :posint) (p :prob))
  (let ((q (- 1 p)))
    (if (and (> (* n p q) 10) (not exact?))
        (let ((z (/ (- p-hat p) (sqrt (/ (* p q) n)))))
          (ecase tails
            (:negative (phi z))
            (:positive (- 1 (phi z)))
            (:both (* 2 (if (<= p-hat p) (phi z) (- 1 (phi z)))))))
        (let* ((observed (round (* p-hat n)))
               (probability-more-extreme
                (if (<= p-hat p)
                    (binomial-cumulative-probability n observed p)
                    (binomial-ge-probability n observed p))))
          (ecase tails
            ((:negative :positive) probability-more-extreme)
            (:both (min (* 2 probability-more-extreme) 1.0)))))))

;; BINOMIAL-TEST-TWO-SAMPLE
;; Rosner 357

;; Are the observed probabilities of an event (p-hat1 and p-hat2) in N1/N2
;; trials different? The normal theory method implemented here.  The exact
;; test is Fisher's contingency table method, below. 

(defun binomial-test-two-sample (p-hat1 n1 p-hat2 n2 &key (tails :both)
                                 (exact? nil))
  (test-variables (p-hat1 :prob) (n1 :posint) (p-hat2 :prob) (n2 :posint))
  (let* ((p-hat (/ (+ (* p-hat1 n1) (* p-hat2 n2)) (+ n1 n2)))
         (q-hat (- 1 p-hat))
         (z (/ (- (abs (- p-hat1 p-hat2)) (+ (/ (* 2 n1)) (/ (* 2 n2))))
               (sqrt (* p-hat q-hat (+ (/ n1) (/ n2)))))))
    (if (and (> (* n1 p-hat q-hat) 5)
             (> (* n2 p-hat q-hat) 5)
             (not exact?))
        (* (- 1 (phi z)) (if (eql tails :both) 2 1))
        (let ((contingency-table (make-array '(2 2))))
          (setf (aref contingency-table 0 0) (* p-hat1 n1)
                (aref contingency-table 0 1) (- 1 (* p-hat1 n1))
                (aref contingency-table 1 0) (* p-hat2 n2)
                (aref contingency-table 1 1) (- 1 (* p-hat2 n2)))
          (fisher-exact-test contingency-table :tails tails)))))
               
;; FISHER-EXACT-TEST
;; Rosner 371
;; Fisher's exact test.  Gives a p value for a particular 2x2 contingency table

(defun fisher-exact-test (contingency-table &key (tails :both))
  (flet ((table-probability (a b c d)
           (let ((n (+ a b c d)))
             (/ (* (factorial (+ a b)) (factorial (+ c d))
                   (factorial (+ a c)) (factorial (+ b d)))
                (* (factorial n) (factorial a) (factorial b)
                   (factorial c) (factorial d))))))

    (let ((a (aref contingency-table 0 0))
          (b (aref contingency-table 0 1))
          (c (aref contingency-table 1 0))
          (d (aref contingency-table 1 1)))
      (test-variables (a number) (b number) (c number) (d number))
      (let* ((row-margin1 (+ a b))
             (row-margin2 (+ c d))
             (column-margin1 (+ a c))
             (column-margin2 (+ b d))
             (n (+ a b c d))
             (table-probabilities
              (make-array (1+ (min row-margin1 row-margin2 column-margin1
                                   column-margin2)))))

        ;; rearrange so that the first row and column marginals are
        ;; smallest.  Only need to change first margins and a.
    
        (cond ((and (< row-margin2 row-margin1) (< column-margin2 column-margin1))
               (psetq a d 
                      row-margin1 row-margin2
                      column-margin1 column-margin2))
              ((< row-margin2 row-margin1)
               (psetq a c
                      row-margin1 row-margin2))
              ((< column-margin2 column-margin1)
               (psetq a b
                      column-margin1 column-margin2)))
        (dotimes (i (length table-probabilities))
          (let* ((test-a i)
                 (test-b (- row-margin1 i))
                 (test-c (- column-margin1 i))
                 (test-d (- n (+ test-a test-b test-c))))
            (setf (aref table-probabilities i)
                  (table-probability test-a test-b test-c test-d))))
        (let ((above (reduce #'+ (subseq table-probabilities 0 (1+ a))))
              (below (reduce #'+ (subseq table-probabilities a))))
          (float
           (ecase tails
             ((:both) (* 2 (min above below)))
             ((:positive) below)
             ((:negative) above))
           1d0))))))

;; MCNEMARS-TEST
;; Rosner 379 and 381

;; McNemar's test for correlated proportions, used for longitudinal
;; studies. Look only at the number of discordant pairs (one treatment is
;; effective and the other is not).  If the two treatments are A and B,
;; a-discordant-count is the number where A worked and B did not, and
;; b-discordant-count is the number where B worked and A did not.

(defun mcnemars-test (a-discordant-count b-discordant-count &key (exact? nil))
  (test-variables (a-discordant-count :posint) (b-discordant-count :posint))
  (let ((n (+ a-discordant-count b-discordant-count)))
    (if (and (> n 20) (not exact?))
        (let ((x2 (/ (square
                      (- (abs (- a-discordant-count b-discordant-count)) 1))
                     n)))
          (- 1 (chi-square-cdf x2 1)))
        (cond ((= a-discordant-count b-discordant-count) 1.0)
              ((< a-discordant-count b-discordant-count)
               (* 2 (binomial-le-probability n a-discordant-count 1/2)))
              (t (* 2 (binomial-ge-probability n a-discordant-count 1/2)))))))

;; POISSON-TEST-ONE-SAMPLE
;; Rosner 256 (approximation on 259)
;; The significance of a one sample test for the equality of an observed
;; number of events (observed) and an expected number mu under the poisson
;; distribution.  Normal theory approximation is not that great, so don't
;; use it unless told.

(defun poisson-test-one-sample (observed mu &key (tails :both) (approximate? nil))
  (test-variables (observed :posnum) (mu :posnum))
  (if approximate?
      (let ((x-square (/ (square (- observed mu)) mu)))
        (- 1 (chi-square-cdf x-square 1)))
      (let ((probability-more-extreme
             (if (< observed mu)
                 (poisson-cumulative-probability mu observed)
                 (poisson-ge-probability mu observed))))
        (ecase tails
          ((:negative :positive) probability-more-extreme)
          (:both (min (* 2 probability-more-extreme) 1.0))))))

;;;
;;; Non-parametric hypothesis testing
;;;

;; SIGN-TEST
;; Rosner 335-7.
;; Really just a special case of the binomial one sample test with p = 1/2.
;; The normal theory version has a correction factor to make it a better
;; approximation. 

(defun sign-test (plus-count minus-count &key (exact? nil) (tails :both))
  (test-variables (plus-count :posint) (minus-count :posint))
  (let* ((n (+ plus-count minus-count))
         (p-hat (/ plus-count n)))
    (if (or (< n 20) exact?)
        (binomial-test-one-sample p-hat n 0.5 :tails tails :exact? t)
        (let ((area (- 1 (phi (/ (1- (abs (- plus-count minus-count)))
                                 (sqrt n))))))
          (if (eql tails :both)
              (* 2 area)
              area)))))

;; SIGN-TEST-ON-SEQUENCE

;; Same as above, but takes two sequences and tests whether the entries in
;; one are different (greater or less) than the other.

(defun sign-test-on-sequences (sequence1 sequence2 &key (exact? nil) (tails :both))
  (test-variables (sequence1 :numseq) (sequence2 :numseq)
              ("Sequences must be of equal length"
               :test (= (length sequence1) (length sequence2))))
  (let* ((differences (map 'list #'- sequence1 sequence2))
         (plus-count (count #'plusp differences))
         (minus-count (count #'minusp differences)))
    (sign-test plus-count minus-count :exact? exact? :tails tails)))

;; WILCOXON-SIGNED-RANK-TEST
;; Rosner 341
;; A test on the ranking of positive and negative differences (are the
;; positive differences significantly larger/smaller than the negative
;; ones). Assumes a continuous and symmetric distribution of differences,
;; although not a normal one.  This is the normal theory approximation,
;; which is only valid when N > 15.

;; This test is completely equivalent to the Mann-Whitney test.

(defun wilcoxon-signed-rank-test (differences &optional (tails :both))
  (let* ((nonzero-differences (remove 0 differences :test #'=))
         (sorted-list (sort (mapcar #'(lambda (dif)
                                        (list (abs dif) (sign dif)))
                                    nonzero-differences)
                            #'<
                            :key #'first))
         (distinct-values (delete-duplicates (mapcar #'first sorted-list)))
         (ties nil))

    (when (< (length nonzero-differences) 16)
      (error "This Wilcoxon Signed-Rank Test (normal approximation method) requires nonzero N > 15"))

    (unless (member tails '(:positive :negative :both))
      (error "tails must be one of :positive, :negative or :both, not ~a" tails))
    
    ; add avg-rank to the sorted values
    
    (dolist (value distinct-values)
      (let ((first (position value sorted-list :key #'first))
            (last (position value sorted-list :key #'first :from-end t)))
        (if (= first last)
            (nconc (find value sorted-list :key #'first) (list (1+ first)))
            (let ((number-tied (1+ (- last first)))
                  (avg-rank (1+ (/ (+ first last) 2)))) ; +1 since 0 based
              (push number-tied ties)
              (dotimes (i number-tied)
                (nconc (nth (+ first i) sorted-list) (list avg-rank)))))))
    (setq ties (nreverse ties))
    (let* ((direction (if (eq tails :negative) -1 1))
           (r1 (reduce #'+ (mapcar #'(lambda (entry)
                                       (if (= (second entry) direction)
                                           (third entry)
                                           0))
                                   sorted-list)))
           (n (length nonzero-differences))
           (expected-r1 (/ (* n (1+ n)) 4))
           (ties-factor (if ties
                            (/ (reduce #'+ (mapcar #'(lambda (ti)
                                                       (- (* ti ti ti) ti))
                                                   ties))
                               48)
                            0))
           (var-r1 (- (/ (* n (1+ n) (1+ (* 2 n))) 24) ties-factor))
           (T-score (/ (- (abs (- r1 expected-r1)) 1/2) (sqrt var-r1))))
      (* (if (eq tails :both) 2 1) (- 1 (phi T-score))))))


(defun wilcoxon-signed-rank-test-on-sequences (sequence1 sequence2
                                               &optional (tails :both))
  (test-variables (sequence1 :numseq) (sequence2 :numseq)
              ("Sequences must be of equal length"
               :test (= (length sequence1) (length sequence2))))
  (wilcoxon-signed-rank-test (map 'list #'- sequence1 sequence2) tails))


;; CHI-SQUARE-TEST-RXC
;; Rosner 395
;; Takes contingency-table, an RxC array, and returns the significance of
;; the relationship between the row variable and the column variable.  Any
;; difference in proportion will cause this test to be significant --
;; consider using the test for trend instead if you are looking for a
;; consistent change.

(defun chi-square-test-rxc (contingency-table)
  (let* ((rows (array-dimension contingency-table 0))
         (columns (array-dimension contingency-table 1))
         (row-marginals (make-array rows :initial-element 0.0))
         (column-marginals (make-array columns :initial-element 0.0))
         (total 0.0)
         (expected-lt-5 0)
         (expected-lt-1 0)
         (expected-values (make-array (list rows columns)
                                      :element-type 'double-float))
         (x2 0.0))
    (dotimes (i rows)
      (dotimes (j columns)
        (let ((cell (aref contingency-table i j)))
          (incf (svref row-marginals i) cell)
          (incf (svref column-marginals j) cell)
          (incf total cell))))
    (dotimes (i rows)
      (dotimes (j columns)
        (let ((expected (/ (* (aref row-marginals i) (aref column-marginals j))
                           total)))
          (when (< expected 1) (incf expected-lt-1))
          (when (< expected 5) (incf expected-lt-5))
          (setf (aref expected-values i j) expected))))
    (when (plusp expected-lt-1)
      (error "This test cannot be used when an expected value is less than one"))
    (when (> expected-lt-5 (/ (* rows columns) 5))
      (error "This test cannot be used when more than 1/5 of the expected values are less than 5."))
    (dotimes (i rows)
      (dotimes (j columns)
        (incf x2 (/ (square (- (aref contingency-table i j)
                               (aref expected-values i j)))
                    (aref expected-values i j)))))
    (- 1 (chi-square-cdf x2 (* (1- rows) (1- columns))))))
                 
            
;; CHI-SQUARE-TEST-FOR-TREND
;; Rosner 398

;; This test works on a 2xk table and assesses if there is an increasing or
;; decreasing trend.  Arguments are equal sized lists counts.  Optionally,
;; provide a list of scores, which represent some numeric attribute of the
;; group.  If not provided, scores are assumed to be 1 to k.

(defun chi-square-test-for-trend (row1-counts row2-counts &optional scores)
  (unless scores (setq scores (dotimes (i (length row1-counts) (nreverse scores))
                                (push (1+ i) scores))))
  (test-variables (row1-counts :posintseq) (row2-counts :posintseq) (scores :numseq)
              ("Sequences must be of equal length"
               :test (= (length row1-counts) (length row2-counts))))
  (let* ((ns (map 'list #'+ row1-counts row2-counts))
         (p-hats (map 'list #'/ row1-counts ns))
         (n (reduce #'+ ns))
         (p-bar (/ (reduce #'+ row1-counts) n))
         (q-bar (- 1 p-bar))
         (s-bar (mean scores))
         (a (reduce #'+ (mapcar (lambda (p-hat ni s)
                                  (* ni (- p-hat p-bar) (- s s-bar)))
                                p-hats ns scores)))
         (b (* p-bar q-bar (- (reduce #'+ (mapcar (lambda (ni s) (* ni (square s)))
                                                  ns scores))
                              (/ (square (reduce #'+ (mapcar (lambda (ni s) (* ni s))
                                                             ns scores)))
                                 n))))
         (x2 (/ (square a) b))
         (significance (- 1 (chi-square-cdf (float x2) 1)))) 
    (when (< (* p-bar q-bar n) 5)
      (error "This test is only applicable when N * p-bar * q-bar >= 5"))
    (format t "~%The trend is ~a, p = ~f"
            (if (< a 0) "decreasing" "increasing")
            significance)
    significance))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sample size estimates
;;;

;; T-TEST-ONE-SAMPLE-SSE
;; Rosner 238
;; Returns the number of subjects needed to test whether the mean of a
;; normally distributed sample mu is different from a null hypothesis mean
;; mu-null and variance variance, with alpha, 1-beta and tails as specified.

(defun t-test-one-sample-sse (mu mu-null variance &key
                                    (alpha 0.05) (1-beta .95) (tails :both))
  (test-variables (mu number) (mu-null number) (variance :posnum)
              (alpha :prob) (1-beta :prob))
  (let ((z-beta (z 1-beta))
        (z-alpha (z (- 1 (if (eql tails :both) (/ alpha 2) alpha)))))
    (round-up (/ (* variance (square (+ z-beta z-alpha)))
                (square (- mu-null mu))))))

;; T-TEST-TWO-SAMPLE-SSE
;; Rosner 308

;; Returns the number of subjects needed to test whether the mean mu1 of a
;; normally distributed sample (with variance variance1) is different from a
;; second sample with mean mu2 and variance variance2, with alpha, 1-beta
;; and tails as specified.  It is also possible to set a sample size ratio
;; of sample 1 to sample 2.

(defun t-test-two-sample-sse (mu1 variance1 mu2 variance2 &key
                                        (sample-ratio 1) (alpha 0.05)
                                        (1-beta .95) (tails :both))
  (test-variables (mu1 number) (variance1 :posnum) (mu2 number)
              (variance2 :posnum) (sample-ratio :posnum) (alpha :prob)
              (1-beta :prob))
  (let* ((delta2 (square (- mu1 mu2)))
         (z-term (square (+ (z 1-beta)
                           (z (- 1 (if (eql tails :both)
                                       (/ alpha 2)
                                       alpha))))))
         (n1 (round-up (/ (* (+ variance1 (/ variance2 sample-ratio)) z-term)
                          delta2)))) 
    (values n1 (round-up (* sample-ratio n1)))))

     
;; T-TEST-PAIRED-SSE
;; Rosner 311

;; Returns the number of subjects needed to test whether the differences
;; with mean difference-mu and variance difference-variance, with alpha,
;; 1-beta and tails as specified.

(defun t-test-paired-sse (difference-mu difference-variance
                                    &key (alpha 0.05) (1-beta 0.95)
                                    (tails :both))
  (test-variables (difference-mu number) (difference-variance :posnum)
              (alpha :prob) (1-beta :prob))
  (round-up (/ (* 2 difference-variance
                 (square (+ (z 1-beta)
                            (z (- 1 (if (eql tails :both)
                                        (/ alpha 2)
                                        alpha))))))
              (square difference-mu))))                                                              


;; BINOMIAL-TEST-ONE-SAMPLE-SSE
;; Rosner 254

;; Returns the number of subjects needed to test whether an observed
;; probability is significantly different from a particular binomial null
;; hypothesis with a significance alpha and a power 1-beta.

(defun binomial-test-one-sample-sse (p-estimated p-null &key
                                               (alpha 0.05) (1-beta 0.95)
                                               (tails :both))
  (test-variables (p-estimated :prob) (p-null :prob) (alpha :prob) (1-beta :prob))
  (let ((q-null (- 1 p-null))
        (q-estimated (- 1 p-estimated)))
    (round-up 
     (/ (* p-null q-null
           (square (+ (z (- 1 (if (eql tails :both) (/ alpha 2) alpha)))
                      (* (z 1-beta)
                         (sqrt (/ (* p-estimated q-estimated)
                                  (* p-null q-null)))))))
        (square (- p-estimated p-null))))))

;; BINOMIAL-TEST-TWO-SAMPLE-SSE
;; Rosner 384

;; The number of subjects needed to test if two binomial probabilities are
;; different at a given significance alpha and power 1-beta.  The sample
;; sizes can be unequal; the p2 sample is sample-sse-ratio * the size of
;; the p1 sample.  It can be a one tailed or two tailed test.  

(defun binomial-test-two-sample-sse (p1 p2 &key (alpha 0.05)
                                               (sample-ratio 1)
                                               (1-beta .95) (tails :both))
  (test-variables (p1 :prob) (p2 :prob) (alpha :prob) (1-beta :prob)
              (sample-ratio :posnum))
  (let* ((q1 (- 1 p1))
         (q2 (- 1 p2))
         (delta (abs (- p1 p2)))
         (p-bar (/ (+ p1 (* sample-ratio p2)) (1+ sample-ratio)))
         (q-bar (- 1 p-bar))
         (z-alpha (z (- 1 (if (eql tails :both) (/ alpha 2) alpha))))
         (z-beta (z 1-beta))
         (n1 (round-up 
              (/ (square (+ (* (sqrt (* p-bar q-bar (1+ (/ sample-ratio))))
                               z-alpha)
                            (* (sqrt (+ (* p1 q1) (/ (* p2 q2) sample-ratio)))
                               z-beta)))
                 (square delta)))))
    (values n1 (round-up (* sample-ratio n1)))))
    

;; BINOMIAL-TEST-PAIRED-SSE
;; Rosner 387

;; Sample size estimate for the McNemar (discordant pairs) test.  Pd is the
;; projected proportion of discordant pairs among all pairs, and Pa is the
;; projected proportion of type A pairs among discordant pairs.  alpha,
;; 1-beta and tails are as above.  Returns the number of individuals
;; necessary; that is twice the number of matched pairs necessary.

(defun binomial-test-paired-sse (pd pa &key (alpha 0.05)
                                 (1-beta 0.95) (tails :both))
  (test-variables (pd :prob) (pa :posnum) (alpha :prob) (1-beta :prob))
  (let ((qa (- 1 pa))
        (z-alpha (z (- 1 (if (eql tails :both) (/ alpha 2) alpha))))
        (z-beta (z 1-beta)))
    (round-up (/ (square (+ z-alpha (* 2 z-beta (sqrt (* pa qa)))))
                 (* 2 (square (- pa 1/2)) pd)))))
                                                                   
;; CORRELATION-SSE
;; Rosner 463
;;
;; Returns the size of a sample necessary to find a correlation of expected
;; value rho with significance alpha and power 1-beta. 

(defun correlation-sse (rho &key (alpha 0.05) (1-beta 0.95))
  (test-variables (rho :prob) (alpha :prob) (1-beta :prob))
  (round-up (+ 3 (/ (square (+ (z (- 1 alpha)) (z 1-beta)))
                    (square (fisher-z-transform rho))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Correlation and Regression
;;;

;; LINEAR-REGRESSION
;; Rosner 431, 441 for t-test

;; Computes the regression equation for a least squares fit of a line to a
;; sequence of points (each a list of two numbers, e.g. '((1.0 0.1) (2.0 0.2)))
;; and report the intercept, slope, correlation coefficient r, R^2, and the
;; significance of the difference of the slope from 0. 

(defun linear-regression (points)
  (test-variables (points sequence))
  (let  ((xs (map 'list #'first points))
         (ys (map 'list #'second points)))
    (test-variables (xs :numseq) (ys :numseq))
    (let* ((x-bar (mean xs))
           (y-bar (mean ys))
           (n (length points))
           (Lxx (reduce #'+ (mapcar (lambda (xi) (square (- xi x-bar))) xs)))
           (Lyy (reduce #'+ (mapcar (lambda (yi) (square (- yi y-bar))) ys)))
           (Lxy (reduce #'+ (mapcar (lambda (xi yi) (* (- xi x-bar) (- yi y-bar)))
                                    xs ys)))
           (b (/ Lxy Lxx))
           (a (- y-bar (* b x-bar)))
           (reg-ss (* b Lxy))
           (res-ms (/ (- Lyy reg-ss) (- n 2)))
           (r (/ Lxy (sqrt (* Lxx Lyy))))
           (r2 (/ reg-ss Lyy))
           (t-test (/ b (sqrt (/ res-ms Lxx)))) 
           (t-significance (t-significance t-test (- n 2) :tails :both)))
      (format t "~%Intercept = ~f, slope = ~f, r = ~f, R^2 = ~f, p = ~f"
              a b r r2 t-significance)
      (values a b r r2 t-significance)))) 

;; CORRELATION-COEFFICIENT
;; just r from above.  Also called Pearson Correlation

(defun correlation-coefficient (points)
  (test-variables (points sequence))
  (let ((xs (map 'list #'first points))
        (ys (map 'list #'second points)))
    (test-variables (xs :numseq) (ys :numseq))
    (let ((x-bar (mean xs))
          (y-bar (mean ys)))
      (/ (reduce #'+ (mapcar #'(lambda (xi yi) (* (- xi x-bar) (- yi y-bar)))
                             xs ys))
         (sqrt (* (reduce #'+ (mapcar #'(lambda (xi) (square (- xi x-bar)))
                                      xs))
                  (reduce #'+ (mapcar #'(lambda (yi) (square (- yi y-bar)))
                                      ys))))))))

;; CORRELATION-TEST-TWO-SAMPLE
;; Rosner 464
;; Test if two correlation coefficients are different.  Users Fisher's Z
;; test.

(defun correlation-test-two-sample (r1 n1 r2 n2 &key (tails :both))
  (test-variables (r1 :prob) (n1 :posint) (r2 :prob) (n2 :posint))
  (let* ((z1 (fisher-z-transform r1))
         (z2 (fisher-z-transform r2))
         (lambda (/ (- z1 z2) (sqrt (+ (/ (- n1 3)) (/ (- n2 3)))))))
    (ecase tails
      (:both (* 2 (if (<= lambda 0) (phi lambda) (- 1 (phi lambda)))))
      (:positive (- 1 (phi lambda)))
      (:negative (phi lambda)))))
         

(defun correlation-test-two-sample-on-sequences (points1 points2 &key (tails :both))
  (test-variables (points1 sequence) (points2 sequence))
  (let ((r1 (correlation-coefficient points1))
        (n1 (length points1))
        (r2 (correlation-coefficient points2))
        (n2 (length points2)))
    (correlation-test-two-sample r1 n1 r2 n2 :tails tails)))

;; SPEARMAN-RANK-CORRELATION
;; Rosner 498

;; Spearman rank correlation computes the relationship between a pair of
;; variables when one or both are either ordinal or have a distribution that
;; is far from normal.   It takes a list of points (same format as
;; linear-regression) and returns the spearman rank correlation coefficient
;; and its significance. 

(defun spearman-rank-correlation (points)
  (test-variables (points sequence))
  (let ((xis (mapcar #'first points))
        (yis (mapcar #'second points)))
    (test-variables (xis :numseq) (yis :numseq))
    (let* ((n (length points))
           (sorted-xis (sort (copy-seq xis) #'<))
           (sorted-yis (sort (copy-seq yis) #'<))
           (average-x-ranks (mapcar (lambda (x) (average-rank x sorted-xis)) xis))
           (average-y-ranks (mapcar (lambda (y) (average-rank y sorted-yis)) yis))
           (mean-x-rank (mean average-x-ranks))
           (mean-y-rank (mean average-y-ranks))
           (Lxx (reduce #'+ (mapcar (lambda (xi-rank) (square (- xi-rank mean-x-rank)))
                                    average-x-ranks)))
           (Lyy (reduce #'+ (mapcar (lambda (yi-rank) (square (- yi-rank mean-y-rank)))
                                    average-y-ranks)))
           (Lxy (reduce #'+ (mapcar (lambda (xi-rank yi-rank)
                                      (* (- xi-rank mean-x-rank)
                                         (- yi-rank mean-y-rank)))
                                    average-x-ranks average-y-ranks)))
           (rs (/ Lxy (sqrt (* Lxx Lyy))))
           (ts (/ (* rs (sqrt (- n 2))) (sqrt (- 1 (square rs)))))
           (p (t-significance ts (- n 2) :tails :both)))
      (format t "~%Spearman correlation coefficient ~f, p = ~f" rs p)
      (values rs p))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Significance functions
;;;

;; T-SIGNIFICANCE
;;  Lookup table in Rosner; this is adopted from CLASP/Numeric Recipes

(defun t-significance (t-statistic dof &key (tails :both))
  "Adopted from CLASP 1.4.3, http://eksl-www.cs.umass.edu/clasp.html"
  (test-variables (t-statistic number) (dof :posint))
  (setf dof (float dof t-statistic))
  (let ((a (beta-incomplete (* 0.5 dof) 0.5 (/ dof (+ dof (square t-statistic))))))
    ;; A is 2*Integral from (abs t-statistic) to Infinity of t-distribution
    (ecase tails
      (:both a)
      (:positive (if (plusp t-statistic)
		     (* .5 a)
		     (- 1.0 (* .5 a))))
      (:negative (if (plusp t-statistic)
		     (- 1.0 (* .5 a))
		     (* .5 a))))))

;; F-SIGNIFICANCE
;; From CLASP

(defun f-significance
       (f-statistic numerator-dof denominator-dof &optional one-tailed-p)
  "Adopted from CLASP, but changed to handle F < 1 correctly in the
one-tailed case.  The `f-statistic' must be a positive number.  The degrees
of freedom arguments must be positive integers.  The `one-tailed-p' argument
is treated as a boolean.

This implementation follows Numerical Recipes in C, section 6.3 and the `ftest'
function in section 13.4."
  (setq f-statistic (float f-statistic))
  (test-variables (f-statistic :posnum) (numerator-dof :posint) (denominator-dof :posint))
  (let ((tail-area (beta-incomplete
		     (* 0.5d0 denominator-dof)
		     (* 0.5d0 numerator-dof)
		     (float (/ denominator-dof
			       (+ denominator-dof
				  (* numerator-dof f-statistic))) 1d0))))
    (if one-tailed-p
        (if (< f-statistic 1) (- 1 tail-area) tail-area)
	(progn (setf tail-area (* 2.0 tail-area))
	       (if (> tail-area 1.0)
		   (- 2.0 tail-area)
		   tail-area)))))

;; CHI-SQUARE and NORMAL (Gaussian) significance are calculated from their
;; respective CDF functions.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities of potential external use
;;;

;; RANDOM-SAMPLE
;; Return a random sample of size N from sequence, without replacement.  If
;; N is equal to or greater than the length of the sequence, return the
;; entire sequence. 

(defun random-sample (n sequence)
  (test-variables (n integer) (sequence sequence))
  (cond ((null sequence) nil)
        ((<= n 0) nil)
        ((< (length sequence) n) sequence)
        (t (let ((one (random-pick sequence)))
             (cons one (random-sample (1- n) (remove one sequence :count 1)))))))

;; RANDOM-PICK
;; Random selection from sequence

(defun random-pick (sequence)
  (test-variables (sequence sequence))
  (when sequence (elt sequence (random (length sequence)))))

;; RANDOM-NORMAL
;; returns a random number with mean and standard-distribution as specified.

(defun random-normal (&key (mean 0) (sd 1))
  (test-variables (mean number) (sd :posnum))
  (let ((random-standard (z (random 1d0))))
    (+ mean (* sd random-standard))))
                              

;; BIN-AND-COUNT

;; Make N equal width bins and count the number of elements of sequence that
;; belong in each.

(defun bin-and-count (sequence n)
  (let* ((min  (reduce #'min sequence))
         (increment (/ (- (reduce #'max sequence) min) n))
         (bins (make-array n :initial-element 0)))
    (dotimes (bin n bins)
      (setf (aref bins bin)
            (count-if #'(lambda (x) (and (>= x (+ min (* bin increment)))
                                         (< x (+ min (* (1+ bin) increment)))))
                      sequence)))))

;; FISHER-Z-TRANSFORM
;; Rosner 458
;; Transforms the correlation coefficient to an approximately normal
;; distribution. 


(defun fisher-z-transform (r)
  (test-variables (r :prob))
  (* 1/2 (log (/ (1+ r) (- 1 r)))))
  
;; PERMUTATIONS
;; How many ways to take n things taken k at a time, when order matters
;; Rosner 88

(defun permutations (n k)
  (test-variables (n :posint) (k :posint)
             ("K must be less than or equal to N" :test (<= k n)))
  (let ((p 1))
    (dotimes (i (1+ k) p)
      (setq p (* p (- n i))))))

;; COMBINATIONS
;; How may ways to take n things taken k at a time, when order doesn't matter
;; Rosner 90

(defun choose (n k) 
  (test-variables (n :posint) 
             ("K must be between 0 and N (inclusive)" :test (and (>= k 0) (<= k n))))
  (/ (factorial n) (* (factorial k) (factorial (- n k)))))

;; MEAN-SD-N
;; A combined calculation that is often useful.  Takes a sequence and
;; returns three values: mean, standard deviation and N.

(defun mean-sd-n (sequence)
  (test-variables (sequence :numseq))
  (values (mean sequence) (standard-deviation sequence) (length sequence)))


;; ROUND-FLOAT
;;
;; Rounds a floating point number to a specified number of digits precision. 


(defun round-float (x &key (precision 5))
  (test-variables (x number) (precision :posint))
  (/ (round x (expt 10 (- precision))) (expt 10 precision)))



;; FALSE-DISCOVERY-CORRECTION
;;
;; A multiple testing correction that is less conservative than Bonferroni.
;; Takes a list of p-values and a false discovery rate, and returns the
;; number of p-values that are likely to be good enough to reject the null
;; at that rate.  Returns a second value which is the p-value cutoff. See
;;
;;   Benjamini Y and Hochberg Y. "Controlling the false discovery rate: a
;;   practical and powerful approach to multiple testing." J R Stat Soc Ser
;;   B 57: 289 300, 1995.


(defun false-discovery-correction (p-values &key (rate 0.05))
  (let ((number-of-tests (length p-values))
        (sorted-p-values (sort p-values #'>)))
    (do ((p-value (pop sorted-p-values) (pop sorted-p-values))
         (tests-to-go number-of-tests (1- tests-to-go)))
        ((or (null p-value)
             (<= p-value (* rate (/ tests-to-go number-of-tests))))
         (values tests-to-go (* rate (/ tests-to-go number-of-tests)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Internal functions
;;;



(defun round-up (x)
  (multiple-value-bind (rounded ignore) (ceiling x)
    (declare (ignore ignore))
    rounded))

(defun sign (x)
  (cond ((minusp x) -1)
        ((plusp x) 1)
        ((zerop x) 0)
        (t nil)))

(defun factorial (number)
  (if (not (and (integerp number) (>= number 0)))
      (error "factorial: ~a is not a positive integer" number)
    (labels ((fact (num) (if (= 0 num) 1 (* num (fact (1- num))))))
       (fact number))))

;; Average rank calculation for non-parametric tests.  Ranks are 1 based,
;; but lisp is 0 based, so add 1!
         
(defun average-rank (value sorted-values)
  (let ((first (position value sorted-values))
        (last (position value sorted-values :from-end t)))
    (1+ (if (= first last)
        first
        (/ (+ first last) 2)))))

;;; CLASP utilities:

;;; Copyright (c) 1990 - 1994 University of Massachusetts
;;; Department of Computer Science
;;; Experimental Knowledge Systems Laboratory
;;; Professor Paul Cohen, Director.
;;; All rights reserved.

;;; Permission to use, copy, modify and distribute this software and its
;;; documentation is hereby granted without fee, provided that the above
;;; copyright notice of EKSL, this paragraph and the one following appear
;;; in all copies and in supporting documentation.
;;; EKSL makes no representation about the suitability of this software for any
;;; purposes.  It is provided "AS IS", without express or implied warranties
;;; including (but not limited to) all implied warranties of merchantability
;;; and fitness for a particular purpose, and notwithstanding any other
;;; provision contained herein.  In no event shall EKSL be liable for any
;;; special, indirect or consequential damages whatsoever resulting from loss
;;; of use, data or profits, whether in an action of contract, negligence or
;;; other tortuous action, arising out of or in connection with the use or
;;; performance of this software, even if EKSL is advised of the possiblity of
;;; such damages.

;;; For more information write to clasp-support@cs.umass.edu

(defun error-function (x)
  "Adopted from CLASP 1.4.3, http://eksl-www.cs.umass.edu/clasp.html"
  ;; per CMUCL type-checker, the results of this function may not be
  ;; correct, if the input isn't a double-float.  For now, I think
  ;; it's easiest to coerce, but later it would be better to ensure
  ;; that the callers do the right thing.
  (let ((erf (gamma-incomplete 0.5d0 (square (coerce x 'double-float)))))
    (if (>= x 0.0d0) erf (- erf))))

;; Bug fix from rpgoldman@siftech.com.  Have to coerce an to a double-float, not a float. 

(defun gamma-incomplete (a x)
  "Adopted from CLASP 1.4.3, http://eksl-www.cs.umass.edu/clasp.html"
  (declare (optimize (safety 3)))
  (setq a (coerce a 'double-float))
  (let ((gln (the double-float (gamma-ln a))))
    (when (= x 0.0)
      (return-from gamma-incomplete (values 0.0d0 gln)))
    (if (< x (+ a 1.0d0))
        ;; Use series representation.  The following is the code of what
        ;; Numerical Recipes in C calls ``GSER'
        (let* ((itmax 1000)
               (eps   3.0d-7)
               (ap    a)
               (sum   (/ 1d0 a))
               (del sum))
          (declare (type double-float ap sum del)
		   (type fixnum itmax))
          (dotimes (i itmax)
            (incf ap 1.0d0)
            (setf del (* del (/ x ap)))
            (incf sum del)
            (if (< (abs del) (* eps (abs sum)))
                (let ((result (underflow-goes-to-zero
                               (* sum (safe-exp (- (* a (log x)) x gln))))))
                  (return-from gamma-incomplete (values result gln)))))
          (error "Series didn't converge:~%~
                  Either a=~s is too large, or ITMAX=~d is too small." a itmax))
        ;; Use the continued fraction representation.  The following is the
        ;; code of what Numerical Recipes in C calls ``GCF.'' Their code
        ;; computes the complement of the desired result, so we subtract from
        ;; 1.0 at the end.
        (let ((itmax 1000)
              (eps   3.0e-7)
              (gold 0d0) (g 0d0) (fac 1d0) (b1 1d0) (b0 0d0)
              (anf 0d0) (ana 0d0) (an 0d0) (a1 x) (a0 1d0))
          (declare (type double-float gold g fac b1 b0 anf ana an a1 a0))
          (dotimes (i itmax)
            (setf an  (coerce (1+ i) 'double-float)
                  ana (- an a)
                  a0  (* fac (+ a1 (* a0 ana)))
                  b0  (* fac (+ b1 (* b0 ana)))
                  anf (* fac an)
                  a1  (+ (* x a0) (* anf a1))
                  b1  (+ (* x b0) (* anf b1)))
            (unless (zerop a1)
              (setf fac (/ 1.0d0 a1)
                    g   (* b1 fac))
              (if (< (abs (/ (- g gold) g)) eps)
                  (let ((result (underflow-goes-to-zero
                                 (* (safe-exp (- (* a (log x)) x gln)) g))))
                    (return-from
                     gamma-incomplete (values (- 1.0d0 result) gln)))
                  (setf gold g))))
          (error "Continued Fraction didn't converge:~%~
                  Either a=~s is too large, or ITMAX=~d is too small." a
                  itmax)))))


(defun gamma-ln (x)
  "Adopted from CLASP 1.4.3, http://eksl-www.cs.umass.edu/clasp.html"
  (cond ((<= x 0) (error "arg to gamma-ln must be positive:  ~s" x))
	((> x 1.0d302)
	 (error "Argument too large:  ~e" x))
	((= x 0.5d0)
	 ;; special case for this arg, since it is used by the error-function
	 (log (sqrt pi)))
	((< x 1)
	 ;; Use reflection formula:  Gamma(1-z) = z*pi/(Gamma(1+z)sin(pi*z))
	 (let ((z (- 1.0d0 x)))
	   (- (+ (log z) (log pi)) (+ (gamma-ln (+ 1.0 z)) (log (sin (* pi z)))))))
	(t (let* ((xx  (- x 1.0d0))
		  (tmp (+ xx 5.5d0))
		  (ser 1.0d0))
	     (declare (type double-float xx tmp ser))
	     (decf tmp (* (+ xx 0.5d0) (log tmp)))
	     (dolist (coef '(76.18009173d0 -86.50532033d0 24.01409822d0
					   -1.231739516d0 0.120858003d-2 -0.536382d-5))
	       (declare (type double-float coef))
	       (incf xx 1.0d0)
	       (incf ser (/ coef xx)))
	     (- (log (* 2.50662827465d0 ser)) tmp)))))

(defun error-function-complement (x)
  "Adopted from CLASP 1.4.3, http://eksl-www.cs.umass.edu/clasp.html"
  (let* ((z   (abs x))
	 (y   (/ 1d0 (+ 1d0 (* 0.5 z))))   ; instead of t
	 (ans
	      (* y (exp (+ (* (- z) z)
			   -1.26551223
			   (* y
			      (+ 1.00002368
				 (* y
				    (+ 0.37409196
				       (* y
					  (+ 0.09678418
					     (* y
						(+ -0.18628806
						   (* y
						      (+ 0.27886807
							 (* y
							    (+ -1.13520398
							       (* y
								  (+ 1.48851587
								     (* y
									(+ -0.82215223
									   (* y 0.17087277))))))))))))))))))))))
    (declare (type double-float z y ans))
    (if (>= x 0.0)
	ans
	(- 2.0 ans))))

(defun find-critical-value
       (p-function p-value &optional (x-tolerance .00001) (y-tolerance .00001))
  "Adopted from CLASP 1.4.3, http://eksl-www.cs.umass.edu/clasp.html"
  (let* ((x-low 0d0)
	 (fx-low 1d0)
	 (x-high 1d0)
	 (fx-high (coerce (funcall p-function x-high) 'double-float)))
    ;; double up
    (declare (type double-float x-low fx-low x-high fx-high))
    (do () (nil)
      ;; for general functions, we'd have to try the other way of bracketing,
      ;; and probably have another way to terminate if, say, y is not in the
      ;; range of f.
      (when (>= fx-low p-value fx-high)
	(return))
      (setf x-low x-high
	    fx-low fx-high
	    x-high (* 2.0 x-high)
	    fx-high (funcall p-function x-high)))
    ;; binary search
    (do () (nil)
      (let* ((x-mid  (/ (+ x-low x-high) 2.0))
	     (fx-mid (funcall p-function x-mid))
	     (y-diff (abs (- fx-mid p-value)))
	     (x-diff (- x-high x-low)))
	(when (or (< x-diff x-tolerance)
		  (< y-diff y-tolerance))
	  (return-from find-critical-value x-mid))
	;; Because significance is monotonically decreasing with x, if the
	;; function is above the desired p-value...
	(if (< p-value fx-mid)
	    ;; then the critical x is in the upper half
	    (setf x-low x-mid
		  fx-low fx-mid)
	    ;; otherwise, it's in the lower half
	    (setf x-high x-mid
		  fx-high fx-mid))))))

(defun beta-incomplete (a b x)
  "Adopted from CLASP 1.4.3, http://eksl-www.cs.umass.edu/clasp.html"
   (flet ((betacf (a b x)
	    ;; straight from Numerical Recipes in C, section 6.3
	    (declare (type double-float a b x))
	    (let ((ITMAX 1000)
		  (EPS   3.0d-7)
		  (qap 0d0) (qam 0d0) (qab 0d0) (em  0d0) (tem 0d0) (d 0d0)
		  (bz  0d0) (bm  1d0) (bp  0d0) (bpp 0d0)
		  (az  1d0) (am  1d0) (ap  0d0) (app 0d0) (aold 0d0))
	      (declare (type double-float qap qam qab em tem d
			     bz bm bp bpp az am ap app aold))
	      (setf qab (+ a b)
		    qap (+ a 1d0)
		    qam (- a 1d0)
		    bz  (- 1d0 (/ (* qab x) qap)))
	      (dotimes (m ITMAX)
		(setf em   (+ 1d0 m)
		      tem  (+ em em)
		      d    (/ (* em (- b em) x)
			      (* (+ qam tem) (+ a tem)))
		      ap   (+ az (* d am))
		      bp   (+ bz (* d bm))
		      d    (/ (* (- (+ a em)) (+ qab em) x)
			      (* (+ qap tem) (+ a tem)))
		      app  (+ ap (* d az))
		      bpp  (+ bp (* d bz))
		      aold az
		      am   (/ ap bpp)
		      bm   (/ bp bpp)
		      az   (/ app bpp)
		      bz   1d0)
		(if (< (abs (- az aold)) (* EPS (abs az)))
		    (return-from betacf az)))
	      (error "a=~s or b=~s too big, or ITMAX too small in BETACF"
		     a b))))
      (declare (notinline betacf))
      (setq a (coerce a 'double-float) b (coerce b 'double-float)
            x (coerce x 'double-float))
      (when (or (< x 0d0) (> x 1d0))
	 (error "x must be between 0d0 and 1d0:  ~f" x))
      ;; bt is the factors in front of the continued fraction
      (let ((bt (if (or (= x 0d0) (= x 1d0))	    
		    0d0
		    (exp (+ (gamma-ln (+ a b))
			    (- (gamma-ln a))
			    (- (gamma-ln b))
			    (* a (log x))
			    (* b (log (- 1d0 x))))))))
	 (if (< x (/ (+ a 1d0) (+ a b 2.0)))
	     ;; use continued fraction directly
	     (/ (* bt (betacf a b x)) a) 
	     ;; use continued fraction after making the symmetry transformation
	     (- 1d0 (/ (* bt (betacf b a (- 1d0 x))) b))))))


(defun safe-exp (x)
  "Eliminates floating point underflow for the exponential function.
Instead, it just returns 0.0d0"
  (setf x (coerce x 'double-float))
  (if (< x (log least-positive-double-float))
      0.0d0
      (exp x)))

;;; </code>
