(in-package :cl-user)

(defpackage :random-distributions
  (:use :cl)
  (:nicknames :randist)
  (:export
   random-uniform
   
   random-gamma
   random-gamma1
   random-gamma-mt
   random-gamma-int

   random-normal
   random-normal-ziggurat

   random-beta
   random-binomial
   random-poisson
   random-multinomial

  
   make-discrete-random-var))
