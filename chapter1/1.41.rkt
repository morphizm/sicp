#lang racket/base

(require rackunit)

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (inc x) (+ 1 x))

; (inc 10)

(define (double proc)
  (lambda (x) (proc (proc x)))
)
((double inc) 1)

(((double (double double)) inc) 1)


(check-equal? (inc 5) 6)
(check-equal? ((double inc) 1) 3)
(check-equal? (((double (double double)) inc) 5) 21)


