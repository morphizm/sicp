#lang racket/base

(require rackunit)

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))
    )
  )
  (iter a null-value)
)

(define (accumulate-recursive combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner null-value (accumulate-recursive combiner (term a) term (next a) next b)) 
  )
)


(define (identity x) x)
(define (inc n) (+ n 1))
(define (sum-integers a b) 
  (define (combiner x y) (+ x y))
  (accumulate combiner 0 identity a inc b)
)

(sum-integers 1 10)

(check-equal? (sum-integers 1 10) 55)
