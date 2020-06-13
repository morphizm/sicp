#lang racket/base

(require "./limitations.rkt")

(define (c+ x y)
  (let [(z (make-connector))]
    (adder x y z)
    z))

(define (c- x y)
  (let [(z (make-connector))]
    (adder z y x)
    z))

(define (c* x y)
  (let [(z (make-connector))]
    (multiplier x y z)
    z))

(define (c/ x y)
  (let [(z (make-connector))]
    (multiplier z y x)
    z))

(define (cv x)
  (let [(z (make-connector))]
    (constant x z)
    z))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))


(define C/ (c/ (cv 9) (cv 5)))
(get-value C/)

(get-value (c- (cv 9) (cv 5)))
(get-value (c+ (cv 9) (cv 5)))


(probe "Celsius" C)
(probe "Fahrenheit" F)
(set-value! C 25 'user)
; (set-value! F 212 'user)
(forget-value! C 'user)
(set-value! F 212 'user)
