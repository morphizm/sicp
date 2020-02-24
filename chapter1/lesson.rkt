#lang racket/base

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)
  )
)

(define (improve guess x)
  (average guess (/ x guess))
)

(define (average x y)
  (/ (+ x y) 2)
)

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001)
)

(define (abs num)
  (cond ((<= num 0) (- num))
    (num)
  )
)

(define (square x) (* x x))

(define (new-if predicate then-clause else-clause)
  (if (predicate) (then-clause)
    (else-clause)
  )
)

(display "\n")

; (sqrt-iter 1 1)
;; Akkerman
(define (A x y)
  (cond
    ((= y 0) 0)
    ((= x 0) (* 2 y))
    ((= y 1) 2)
    (else (A (- x 1) (A x (- y 1))))
  )
)

; (A 1 10)
; (A 2 4)
; (A 3 3)

(define (count-change amount)
  (cc amount 5)
)

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
    ((or (< amount 0) (= kinds-of-coins 0)) 0)
    (else (+ (cc amount (- kinds-of-coins 1)) (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))
  )
)

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
    ((= kinds-of-coins 2) 5)
    ((= kinds-of-coins 3) 10)
    ((= kinds-of-coins 4) 25)
    ((= kinds-of-coins 5) 50)
  )
)
; (display "COUNT CHANGE ")
; (count-change 11)

(define (fast-expt b n) 
  (cond ((<= n 0) 1)
    ((even? n) (square (fast-expt b (/ n 2)) ))
    (else (* b (fast-expt b (- n 1))))
  )
)
(define (even? n)
  (= (remainder n 2) 0)
)

; (fast-expt 2 8)

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))
  )
)

(define (smallest-divisor n)
  (find-divisor n 2)
)

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))
  )
)

(define (divides? a b)
  (= (remainder b a) 0)
)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
    ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
    (else  (remainder (* base (expmod base (- exp 1) m)) m))
  )
)

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a)
  )
  (try-it (+ 1 (random (- n 1))))
)

(fermat-test 4)