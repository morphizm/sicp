#lang racket/base

(require rackunit)

(define (square x) (* x x))

(define (average a b)
  (/ (+ a b) 2)
)

(define (sqrt x)
  (define (sqrt-iter last-guess guess)
    (if (good-enough? last-guess guess)
      guess
      (sqrt-iter guess (improve guess))
    )
  )
  (define (improve guess)
    (average guess (/ x guess))
  )
  (define (good-enough? last-guess guess)
    (< (abs (- last-guess guess)) 0.001)
  )
  (sqrt-iter 0 1.0)
)

(define (fi) (/ (+ 1 (sqrt 5)) 2))
(define (psi) (/ (- 1 (sqrt 5)) 2))
(fi) ; 1.6180
(psi) ; 0.6180
(square (fi)) ; 2.6180

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))
    )
  )
  (fib-iter 1 0 n)
)
(fib 5)

(check-equal? (fib 5) 5)

