#lang racket/base

(provide gcd square)

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (square x)
  (* x x))