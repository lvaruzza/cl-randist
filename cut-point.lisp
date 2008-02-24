(in-package :randist)

(declaim (optimize (debug 3)))

(defun setup-cut-point-randist (p)
  (let* ((L 0)
	 (J 0)
	 (M (length p))
	 (S 0d0)
	 (I (make-array M))
	 (Qj 0d0)
	 (Q (map 'vector #'(lambda (x) (setf S (+ S x))) p)))


  (tagbody
   start
     (incf j)
     (setf Qj (* M (aref Q (1- J))))
   test
     (when (<= Qj L)
       (go start))
     (incf L)
     (setf (aref I (1- L)) J)
     (when (< L M)
       (go test)))
  (values I Q)))

(defun make-discrete-monotone-random-var (p)
  (multiple-value-bind (I Q) (setup-cut-point-randist p)
    (let ((M (length p)))
      #'(lambda ()
	  (let* ((U (random-uniform))
		 (X (aref I (floor (* M U)))))
	    (tagbody
	     start
	       (if (<= U (aref Q (1- X)))
		   (go end)
		   (progn
		     (incf X)
		     (go start)))
	     end)
	    (1- X))))))
	       
	 
     
