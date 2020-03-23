#lang racket/base

(require rackunit)

(define (make-accumulator value)
  (define (increment sum)
    (begin (set! value (+ value sum)))
            value)
  increment)


(define A (make-accumulator 3))
(define B (make-accumulator 33))
(check-equal? (A 10) 13)
(check-equal? (B 33) 66)
(check-equal? (A 2) 15)
