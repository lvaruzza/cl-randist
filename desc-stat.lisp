(in-package :kempbasu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Descriptive statistics
;;;

;; MEAN 
;; Rosner 10 

(defun mean (sequence)
  (/ (reduce #'+ sequence) (length sequence)))

;; MEDIAN
;; Rosner 12 (and 19)

(defun median (sequence)
  (percentile sequence 50))

;; MODE
;; Rosner 14
;; returns two values: a list of the modes and the number of times they
;; occur.   Rob St. Amant <stamant@csc.ncsu.edu> suggested using a hash
;; table instead of an alist, and Lee Ayres <ayres@acm.org> noted that 
;; my revision failed to handle multiple modes properly.

(defun mode (sequence)
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
  (expt base (mean (map 'list #'(lambda (x) (log x base)) sequence))))

;; RANGE
;; Rosner 18

(defun range (sequence)
  (- (reduce #'max sequence) (reduce #'min sequence)))

;; PERCENTILE
;; Rosner 19
;; NB: Aref is 0 based!

(defun percentile (sequence percent)
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
  (let ((mean (mean sequence))
        (n (length sequence)))
    (/ (reduce #'+ (map 'list #'(lambda (x) (square (- mean x))) sequence))
       (1- n))))

;; STANDARD-DEVIATION (SD)
;; Rosner 21

(defun standard-deviation (sequence)
  (let ((mean (mean sequence))
        (n (length sequence)))
    (sqrt (/ (reduce #'+ (map 'list #'(lambda (x) (square (- mean x))) sequence))
             (1- n)))))


(defun sd (sequence)
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

