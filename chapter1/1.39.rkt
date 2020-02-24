#lang racket/base

(require rackunit)

(define (square x) (* x x))

(define (tan-cf x k)
  (define (tan-cf-rec x k i num)
    (cond ((= k i) 0)
      ((= i 0) (/ x (- num (tan-cf-rec x k (+ i 1) (+ num 2)))))
      (else (/ (square x) (- num (tan-cf-rec x k (+ i 1) (+ num 2)))))
    )
  )
  (tan-cf-rec x k 0 1)
)

(tan-cf 1.0 10)
(tan 1)

(tan-cf 55.0 100)
(tan 55)
