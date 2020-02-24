#lang racket/base

(require rackunit)

(define (square x) (* x x))

(define (fast-expt b n) 
  (cond ((<= n 0) 1)
    ((even? n) (square (fast-expt b (halve n))))
    (else (* b (fast-expt b (- n 1))))
  )
)

(define (even? n)
  (= (remainder n 2) 0)
)

(define (* a b) 
  (if (= b 0)
  0
  (+ a (* a (- b 1))))
)

(define (double num)
  (* 2 num)
)

(define (halve num)
  (/ num 2)
)

(*  9 1)

(fast-expt 2 6)

(check-equal? (fast-expt 2 1) 2)
(check-equal? (fast-expt 2 2) 4)
(check-equal? (fast-expt 2 3) 8)
(check-equal? (fast-expt 2 4) 16)
(check-equal? (fast-expt 2 5) 32)
(check-equal? (fast-expt 2 6) 64)
(check-equal? (fast-expt 2 7) 128)
(check-equal? (fast-expt 2 8) 256)
(check-equal? (fast-expt 2 9) 512)
(check-equal? (fast-expt 2 10) 1024)
(check-equal? (fast-expt 3 3) 27)
(check-equal? (fast-expt 3 6) 729)
(check-equal? (fast-expt 4 12) 16777216)
(check-equal? (fast-expt 10 7) 10000000)