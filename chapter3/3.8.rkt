#lang racket/base

(require rackunit)

(define global -10)

(define (set-global x)
  (begin (set! global x)
    global))

(define (f x)
  (+ (set-global 1) (set-global 0))
  (if (= global 0)
    0
    1
  )
)

(+ (f 0) (f 1))
(display "global = ")
(displayln global)


; applicative or normal ---- (test 0 (p))
(define (p) (p))
(define (test x y)
  (if (= x 0)
    x
    y))
