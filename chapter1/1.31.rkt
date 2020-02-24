#lang racket/base

(require rackunit)

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))
    )
  )
  (iter a 1)
)

(define (product-recursive term a next b)
    (if (> a b)
      1
      (* (term a) (product term (next a) next b))
    )
)

(define (identity x) x)
(define (inc n) (+ n 1))
(define (sum-integers a b) (product identity a inc b))
(sum-integers 1 10)

(define (pi-on-4)
  (define (next n) (+ n 2))
  (define (f a)
    (/ (* a (+ a 2))
      (* (+ a 1) (+ a 1))
    )
  )
  (product f 2.0 next 1000)
)

(pi-on-4)
(/ 3.14 4)