#lang racket/base

(require "./limitations.rkt")

(define (averager a b c)
  (define u (make-connector))
  (define v (make-connector))
  (constant 0.5 u)
  (adder a b v)
  (multiplier u v c))

(define A (make-connector))
(define B (make-connector))
(define C (make-connector))

(probe "A" A)
(probe "B" B)
(probe "C" C)

; it is work
(averager A B C)
(set-value! A 10 'user)
(set-value! B 100 'user)
