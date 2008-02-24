(defpackage #:cl-randist-system
  (:use :cl :asdf))

(in-package #:cl-randist-system)

(defsystem :cl-randist
  :description "Random Distribution Generation"
  :version "0.2"
  :author "Leonardo Varuzza <varuzza@gmail.com>"
  :license "GPLv3"
  :serial t
  :components ((:file "packages")
	       (:file "randist")
	       (:file "normal")
	       (:file "gamma")
	       (:file "beta")
	       (:file "binomial")
	       (:file "poisson")
	       (:file "nbinomial")
	       (:file "multinomial")
	       (:file "alias_method")
	       (:file "exponential")
	       (:file "f")
	       (:file "pareto")
	       (:file "cut-point")
	       (:file "desc-stat")
	       (:file "tests")))

	       