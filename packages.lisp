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
   random-negetive-binomial

   random-poisson
   random-exponential
   random-multinomial

   ;; Alias method for discrete distributions
   make-discrete-random-var))
