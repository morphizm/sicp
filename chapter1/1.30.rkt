#lang racket/base

(require rackunit)

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))
    )
  )
  (iter a 0)
)

(define (identity x) x)
(define (inc n) (+ n 1))
(define (sum-integers a b) (sum identity a inc b))
(sum-integers 1 10)

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))

  (* (sum f (+ a (/ dx 2)) add-dx b) dx)
)

(define (cube x) (* x x x))
(integral cube 0 1 0.01)
