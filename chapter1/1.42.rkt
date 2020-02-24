#lang racket/base

(require rackunit)

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (inc x) (+ 1 x))

; (inc 10)

(define (double proc)
  (lambda (x) (proc (proc x)))
)
; ((double inc) 1)

(define (compose f g)
  (lambda (x) (f (g x)))
)


(check-equal? ((compose square inc) 6) 49)


