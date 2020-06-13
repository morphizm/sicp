#lang racket/base

(require "./limitations.rkt")

(define (squarer a b)
  (multiplier a a b))

(define A (make-connector))
(define B (make-connector))
(probe "A" A)
(probe "B" B)
(squarer A B)

; (set-value! A 10 'user) ; it is work
; (set-value! B 100 'user) ; it is not work because A not execute