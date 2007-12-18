(defpackage #:cl-randist-system
  (:use :cl :asdf))

(in-package #:cl-randist-system)

(defsystem :cl-randist
  :description "Random Distribution Generation"
  :version "0.1"
  :author "Leonardo Varuzza <varuzza@gmail.com>"
  :license "GPLv3"
  :serial t
  :components ((:file "packages")
	       (:file "randist")
	       (:file "normal-ziggurat")
	       (:file "randist-gamma")
	       (:file "randist-gamma-mt")))
	       