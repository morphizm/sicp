#lang racket/base

(require rackunit)

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))
  )
)

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2)
)

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (next test-divisor)))
  )
)

(define (divides? a b)
  (= (remainder b a) 0)
)

(define (prime? n)
  (if (= n 1)
    #f
    (= n (smallest-divisor n))
  )
)

(define (even? n)
  (= (remainder n 2) 0)
)

(define (next num)
  (if (= num 2)
    3
    (+ num 2)
  )
)

(define (filtered-accumulate combiner null-value predicat term a following b)
  (define (iter a result)
    (cond ((> a b) result)
      ((predicat a) (iter (following a) (combiner (term a) result)))
      (else (iter (following a) result))
    )
  )
  (iter a null-value)
)

(define (identity x) x)
(define (inc n) (+ n 1))
(define (sum-integers a b) 
  (define (combiner x y) (+ x y))
  (define (predicat n) #t)
  (filtered-accumulate combiner 0 predicat identity a inc b)
)

(define (sum-square-primes a b) 
  (define (combiner x y) (+ x y))
  (define (f x) (square x))
  (filtered-accumulate combiner 0 prime? f a inc b)
)

(define (products a b)
  (define (combiner x y) (* x y))
  (define (predicat n) (= (gcd n b) 1))
  (filtered-accumulate combiner 1 predicat identity a inc b)
)

(check-equal? (products 1 10) 189)

(check-equal? (sum-integers 1 10) 55)

(check-equal? (sum-square-primes 1 100) 65796)
(check-equal? (sum-square-primes 1 10) 87)
