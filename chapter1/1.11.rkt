#lang racket/base

(require rackunit)

(define (f n)
    (define (f-iter acc x) 
        (if (> x 3) acc
            (f-iter (+ acc (* x (f (- n x)))) (+ x 1))
        )
    )
    (if (< n 3) n (f-iter 0 1))
)

(check-equal? (f 1) 1)
(check-equal? (f 2) 2)
(check-equal? (f 3) 4)
(check-equal? (f 4) 11)
(check-equal? (f 5) 25)
(check-equal? (f 6) 59)
(check-equal? (f 7) 142)