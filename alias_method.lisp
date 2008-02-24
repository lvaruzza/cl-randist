(in-package :randist)

;; An implementation of the Alias Method.
;;
;; NOTE: Additional comments, and an updated, more accurate version
;; can be found here:
;; <http://prxq.wordpress.com/2006/04/23/more-on-the-alias-method/>
;;
;; The function MAKE-DISCRETE-RANDOM-VAR takes an array of
;; probabilities and an (optional) array of values. Produces a
;; function which returns each of the values with the specified
;; probability (or the corresponding integer no values have been
;; given).
;;
;; It needs only one call to RANDOM for every value produced.
;;
;; For the licence, see at the end of the file.
(defun create-alias-method-vectors (probabilities)
  (let* ((N (length probabilities))
	 (threshold (/ 1.0d0 N))

	 (alternatives (make-array N :element-type 'fixnum))
	 
	 (p_keep (make-array N
			     :initial-contents
			     (coerce probabilities 'list)))
	 bigs lows)
    
    (loop :for i :from 0 :below N :do
	  (if (>= threshold (aref probabilities i))
	      (push i lows)
	      (push i bigs)))

    (loop :while lows :do
	  (let* ((0ne (pop lows))
		 (Tw0 (pop bigs))
		 (delta (- threshold
			   (aref p_keep 0ne))))

	    (if tw0
		(progn
		  
		  (setf (aref p_keep 0ne)
			(/ (aref p_keep 0ne) threshold))
		  
		  (setf (aref alternatives 0ne) Tw0)
		  
		  (decf (aref p_keep Tw0) delta)
		  
		  (if (>= threshold (aref p_keep Tw0))
		      (push Tw0 lows)
		      (push Tw0 bigs)))
		(setf (aref p_keep 0ne) 1.0d0))))

    ;; Numerical noise might leave some bigs dangling, with
    ;; | p_keep - 1/N | very small.
    (dolist (k bigs)
      (setf (aref p_keep k) 1.0d0))

    (values p_keep alternatives)))

(defun make-discrete-random-var (probabilities &optional values)
  (when (and values (/= (length values) (length probabilities)))
    (error "different number of values and probabilities."))

  (let* ((N (float (length probabilities) 0.0d0)))
    (multiple-value-bind (p_keep alternatives)
	(create-alias-method-vectors probabilities)
      #'(lambda ()
	  (labels ((result (k)
		     (if values (aref values k) k)))

	    (multiple-value-bind (k r)
		(floor (random-mt N))
	      (if (> r (aref p_keep k))
		  (result (aref alternatives k))
		  (result k))))))))

;; Tests the alias method. p holds the prescribed probabilities, and
;; cnt the measured ones.
(defun test-alias-method (n runs)
  (let ((p (make-array n :element-type 'double-float))
	(cnt (make-array n :initial-element 0.0d0)))

    (dotimes (i n)
      (setf (aref p i) (random-uniform)))
    
    (let ((nc (loop for i from 0 below n summing (aref p i))))

      (dotimes (i n)
	(setf (aref p i)
	      (/ (aref p i) nc)))

      (let ((rp (make-discrete-random-var p)))

	(dotimes (i runs)
	  (incf (aref cnt (funcall rp))))

	(dotimes (i n)
	  (setf (aref cnt i)
		(/ (aref cnt i) runs)))))

    (loop
       for i from 0 to 10
       for j = (random-mt n)
       do (progn
	    (format t "~d:~6t~5e~6t~5e~%" j (aref p j) (aref cnt j))))

    (let ((res2 (loop 
		   for i from 0 to (1- n)
		   for res = (- (aref p i) (aref cnt i))
		   sum (* res res) into acc
		   finally (progn 
			     (format t "acc:~a ~a ~%" acc n)
			     (return acc)))))
      
      (sqrt (/ res2 n)))))



(defun report-uniform-count (c N)
    (loop for x across c 
	 when (= x 1) sum 1 into g1
	 when (= x 0) sum 1 into g0
	 when (and (> x 1) (<= x 2)) sum 1 into g2
	 when (and (> x 2) (<= x 10)) sum 1 into g10
	 when (> x 10) sum 1 into g>
	 sum x into total
	 finally (return (mapcar #'(lambda (x) (float (/ x N))) (list total g0 g1 g2 g10 g>)))))

(defun uniform-reference (N)
  (let* ((c (make-array N :initial-element 0)))

    (loop for i from 1 to N
	 do (incf (aref c (random-mt (1- N)))))
    (report-uniform-count c N)))


(defun test-alias-method-uniform (N)
  (let* ((p (make-array N :initial-element (/ 1.0d0 N)))
	 (c (make-array N :initial-element 0))
	 (f (make-discrete-random-var p)))

    (loop for i from 1 to N
	 do (incf (aref c (funcall f))))

    (let ((uniform (uniform-reference N))
	  (alias (report-uniform-count c N))
	  (multinomial (report-uniform-count (random-multinomial N p) N)))
      (list :uniform uniform :multinomial multinomial :alias alias))))


(defun test-alias-method-zeros ()
  (let* ((p #(0.0d0 0.75d0 0.24d0 0.001d0 0.001d0 0.001d0 0.001d0 0.001d0 0.005d0))
	 (f (make-discrete-random-var p)))

    (format t "1.P = ~f~%" (loop for x across p sum x))

    (loop for i from 0 to 100000
	 for x = (funcall f)
	 when (= x 0) sum 1 into zeros
	 when (= x 1) sum 1 into ones
	 when (= x 4) sum 1 into four
	 finally (return (list zeros ones four)))))
    
;;; Copyright (c) 2006, Mario S. Mommer <m_mommer@yahoo.com>

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
