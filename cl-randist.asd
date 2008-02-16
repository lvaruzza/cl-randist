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
	       (:file "randist-normal")
	       (:file "randist-gamma")
	       (:file "randist-beta")
	       (:file "randist-binomial")
	       (:file "desc-stat")
	       (:file "randist-multinomial")
	       (:file "alias_method")
	       (:file "tests")))

	       