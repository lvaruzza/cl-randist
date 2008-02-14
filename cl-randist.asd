(defpackage #:cl-randist-system
  (:use :cl :asdf))

(in-package #:cl-randist-system)

(defsystem :cl-randist
  :description "Random Distribution Generation"
  :depends-on (:cl-desc-stat)
  :version "0.1"
  :author "Leonardo Varuzza <varuzza@gmail.com>"
  :license "GPLv3"
  :serial t
  :components ((:file "packages")
	       (:file "randist")
	       (:file "randist-normal")
	       (:file "randist-gamma")
	       (:file "randist-beta")
	       (:file "randist-binomial")
	       (:file "randist-multinomial")
	       (:file "tests")))

	       