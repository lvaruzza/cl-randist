(defpackage #:cl-randist-system
  (:use :cl :asdf))

(in-package #:cl-randist-system)

(defsystem :cl-randist
  :description "Random Distribution Generation"
  :version "0.4.1"
  :author "Leonardo Varuzza <varuzza@gmail.com>"
  :license "GPLv3"
  :serial t
  :components ((:file "packages")
	       ;;  #-(or sbcl cmucl) (:file "jmt")
	       ;; originally, we used the internal random.  However,
	       ;; the Common Lisp spec doesn't really support directed
	       ;; restarts for ensuring that the number stream can be
	       ;; made identical.  After all, the importance of that
	       ;; use-case is only a relatively recent phenomena,
	       ;; driven by statistical methodology.
	       (:file "jmt")
	       (:file "randist")
	       (:file "normal")
	       (:file "gamma")
	       (:file "dirichlet")
	       (:file "beta")
	       (:file "binomial")
	       (:file "poisson")
	       (:file "nbinomial")
	       (:file "multinomial")
	       (:file "alias_method")
	       (:file "exponential")
	       (:file "f")
	       (:file "pareto")
	       (:file "chisq")
	       (:file "t")
	       (:file "cut-point")
	       (:file "desc-stat")
	       (:file "GIG")
	       (:file "tests")))
