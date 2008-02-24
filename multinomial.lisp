(in-package :randist)

(declaim (optimize (speed 0) (safety 3) (debug 3)))


#| The multinomial distribution has the form

                                      N!           n_1  n_2      n_K
   prob(n_1, n_2, ... n_K) = -------------------- p_1  p_2  ... p_K
                             (n_1! n_2! ... n_K!) 

   where n_1, n_2, ... n_K are nonnegative integers, sum_{k=1,K} n_k = N,
   and p = (p_1, p_2, ..., p_K) is a probability distribution. 

   Random variates are generated using the conditional binomial method.
   This scales well with N and does not require a setup step.

   Ref: 
   C.S. David, The computer generation of multinomial random variates,
   Comp. Stat. Data Anal. 16 (1993) 205-217


void
gsl_ran_multinomial (const gsl_rng * r, const size_t K,
                     const unsigned int N, const double p[], unsigned int n[])
{
  size_t k;
  double norm = 0.0;
  double sum_p = 0.0;

  unsigned int sum_n = 0;

  /* p[k] may contain non-negative weights that do not sum to 1.0.
   * Even a probability distribution will not exactly sum to 1.0
   * due to rounding errors. 
   */

  for (k = 0; k < K; k++)
    {
      norm += p[k];
    }

  for (k = 0; k < K; k++)
    {
      if (p[k] > 0.0)
        {
          n[k] = gsl_ran_binomial (r, p[k] / (norm - sum_p), N - sum_n);
        }
      else
        {
          n[k] = 0;
        }

      sum_p += p[k];
      sum_n += n[k];
    }

}
|#

(defun random-multinomial1 (NN p n)
  (let ((norm 0d0)
	(k  (1- (array-dimension p 0))))

    (declare (type integer NN))
;;	     (type (simple-array double-float (*)) p))
    
    (setf norm (loop for i fixnum from 0 to k
		    sum (aref p i)))

    (loop for i from 0 to k
	 do (progn
	      (setf (aref n i)
		    (if (> (aref p i) 0d0)
			(random-binomial (/ (aref p i)
					    (- norm sum-p))
					 (- NN sum-n))
			0)))
	 sum (aref p i) into sum-p
	 sum (aref n i) into sum-n)))
		    

(defun random-multinomial (NN p)
  (let ((n (make-array (array-dimension p 0)
		       :element-type 'integer
		       :adjustable nil)))

    (random-multinomial1 NN p n)
    n))

(defun test-multinomial1 (nn p &optional (k 10000))
  (let* ((d (array-dimension p 0))
	 (r (make-array d :initial-element nil ))
	 (n (make-array d :element-type 'integer :adjustable nil)))
    (loop for i from 0 to k
	 do (progn
	      (random-multinomial1 nn p n)
	      (loop for j from 0 to (1- d)
		  do  (push (aref n j) (aref r j)))))
    (loop for j from 0 to (1- d)
	 do (format t "~2d ~8f ~8f~t~8f ~8f~%"
		    j
		    (float (mean (aref r j)))
		    (* nn (aref p j))
		    (float (var (aref r j)))
		    (* nn (aref p j) (- 1d0 (aref p j))))))
  (terpri))

(defun test-multinomial (&optional (k 10000))
  (test-multinomial1 100 #(0.7d0 0.2d0 0.1d0) k)
  (test-multinomial1 1000000 #(0.7d0 0.2d0 0.1d0) k)
  (test-multinomial1 1000000 #(0.7d0 0.2d0 0.08d0 0.01d0 0.005d0 0.005d0) k))
