#lang racket/base

(require rackunit)

(define (square x)
  (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))

(define (sum-of-squares-of-top-two a b c)
  (cond ((and (< a b) (< a c)) (sum-of-squares b c))
    ((and (< b c) (< b a)) (sum-of-squares a c))
    ((sum-of-squares a b))
  )
)

(check-equal? (sum-of-squares-of-top-two 1 2 3) 13)
(check-equal? (sum-of-squares-of-top-two 3 4 2) 25)
(check-equal? (sum-of-squares-of-top-two 5 3 4) 41)
(check-equal? (sum-of-squares-of-top-two 5 5 4) 50)
(check-equal? (sum-of-squares-of-top-two 4 5 5) 50)
