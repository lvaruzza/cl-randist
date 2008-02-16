(in-package :cl-user)

(defpackage :random-distributions
  (:use :cl)
  (:nicknames :randist)
  (:export
   random-uniform
   
   random-gamma
   random-gamma1
   random-gamma-mt

   random-normal
   random-normal-ziggurat

   random-beta
   random-binomial
   random-multinomial

   make-discrete-random-var))
