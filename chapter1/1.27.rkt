#lang racket/base

(require rackunit)

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
  (= n (smallest-divisor n))
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

(define (expmod base exp m)
  (cond ((= exp 0) 1)
    ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
    (else  (remainder (* base (expmod base (- exp 1) m)) m))
  )
)

(define (fermat-test n count)
  (define (try-it a)
    (= (expmod a n n) a)
  )
  (try-it count)
)

(define (karmaikl-test n)
  (define (iter count)
    (cond 
      ((>= count n))
      ((fermat-test n count) (iter (+ count 1)))
      (else
        (display "FALSE")
        (= 0 1)
      ) ; false
    )
  )
  (iter 2)
)

(karmaikl-test 561)
(karmaikl-test 1105)
(karmaikl-test 1729)
(karmaikl-test 2465)
(karmaikl-test 2821)
(karmaikl-test 6601)
(karmaikl-test 9)