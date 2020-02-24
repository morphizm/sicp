#lang racket/base

(require rackunit)

(define (pow a n)
  (if (= n 0)
    1
    (* a (pow a (- n 1)))
  )
)

(define (encode num divider)
  (define (iter newnum acc)
    (if (not (= (remainder newnum divider) 0))
      acc
      (iter (/ newnum divider) (+ acc 1))
    )
  )
  (iter num 0)
)

(define (cons a b)
  (* (pow 2 a) (pow 3 b))
)

(define (car pair)
  (encode pair 2)
)

(define (cdr pair)
  (encode pair 3)
)

(define z (cons 111 213))
(car z)
(cdr z)
(define z1 (cons 0 0))
(define z2 (cons 1 0))

(check-equal? (car z) 111)
(check-equal? (cdr z) 213)
(check-equal? (car z1) 0)
(check-equal? (cdr z1) 0)
(check-equal? (car z2) 1)
(check-equal? (cdr z2) 0)