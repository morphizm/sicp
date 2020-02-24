#lang racket/base

(require rackunit)
(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
      (accumulate op initial (cdr sequence)))
  )
)

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
    0
    coefficient-sequence
  )
)

(horner-eval 2 (list 1 3 0 5 0 1)) ; 79
; Например, если обозначить многочлен как p, 
; то p(1, 3, 0, 5, 0, 1)(x) = 1 + x * p(3, 0, 5, 0, 1)(x).