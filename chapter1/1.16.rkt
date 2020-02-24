#lang racket/base

(require rackunit)

(define (square x) (* x x))

(define (fast-expt-iter b n acc) 
  (cond ((= n 0) acc)
    ((= n 2) (* acc (square b)))
    ((even? n) (fast-expt-iter b (/ n 2) (square (* acc b))) )
    (else (fast-expt-iter b (- n 1) (* acc b)))
  )
)
(define (even? n)
  (= (remainder n 2) 0)
)

; (fast-expt-iter 2 6 1)

(check-equal? (fast-expt-iter 2 1 1) 2)
(check-equal? (fast-expt-iter 2 2 1) 4)
(check-equal? (fast-expt-iter 2 3 1) 8)
(check-equal? (fast-expt-iter 2 4 1) 16)
(check-equal? (fast-expt-iter 2 5 1) 32)
(check-equal? (fast-expt-iter 2 6 1) 64)
(check-equal? (fast-expt-iter 2 7 1) 128)
(check-equal? (fast-expt-iter 2 8 1) 256)
(check-equal? (fast-expt-iter 2 9 1) 512)
(check-equal? (fast-expt-iter 2 10 1) 1024)
; (check-equal? (fast-expt-iter 3 3 1) 27)
; (check-equal? (fast-expt-iter 3 6 1) 729)
; (check-equal? (fast-expt-iter 4 12 1) 16777216)
; (check-equal? (fast-expt-iter 10 7 1) 10000000)
