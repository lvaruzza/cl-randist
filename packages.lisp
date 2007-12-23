(in-package :cl-user)

(defpackage :random-distributions
  (:use :cl :desc-stat)
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
   random-multinomial))
