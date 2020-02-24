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

(define (repeated f n)
  (lambda (x)
    (if (= n 1)
      (f x)
      ((repeated f (- n 1)) (f x))
    )
  )
)

