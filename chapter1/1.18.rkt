#lang racket/base

(require rackunit)

(define (double-double num acc)
  (cond ((= num 0) acc)
    (else (double-double (- num 1) (+ acc (double num))))
  )
)

(define (mult a b acc)
  (cond ((= b 0) acc)
    ((even? b) (mult a (halve b) (+ acc (double a))))
    (else (mult a (- b 1) (+ acc a)))
  )
)

(define (even? n)
  (= (remainder n 2) 0)
)

(define (double num)
  (+ num num)
)

(define (halve num)
  (/ num 2)
)

(mult 9 1 0)

(check-equal? (mult 2 1 0) (* 2 1))
(check-equal? (mult 2 10 0) (* 2 10))
(check-equal? (mult 3 7 0) (* 3 7))