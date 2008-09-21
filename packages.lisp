(in-package :cl-user)

(defpackage :random-distributions
  (:use :cl)
  (:nicknames :randist :cl-randist)
  (:export
   random-uniform

   random-normal
   random-normal-ziggurat
   
   random-gamma
   random-gamma1
   random-gamma-mt
   random-gamma-int

   random-dirichlet

   random-beta
   random-binomial
   random-negative-binomial

   random-poisson
   random-exponential
   random-multinomial
   random-f
   random-pareto

   random-GIG
   random-generalized-inverse-gaussian

   ;;random-logistic (untested)

   random-chi-square

   ;; random-t (untested)

   ;; Alias method for discrete distributions
   make-discrete-random-var

   ;; Cut-point method for discrete distributions
   make-discrete-monotone-random-var
   ))
