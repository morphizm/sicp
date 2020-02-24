#lang racket/base

(require rackunit)


(define (cont-frac n d k i)
  (cond ((= i k) (/ (n i) (d i)))
    (else (/ (n i) (+ (d i) (cont-frac n d k (+ i 1)))))
  )
)

(define (cont-frac-iter n d k i)
  (cond ((= i k) (/ (n i) (d i)))
    (else (/ (n i) (+ (d i) (cont-frac-iter n d k (+ i 1)))))
  )
)
; ?????


(cont-frac (lambda (i) 1.0)
  (lambda (i) 1.0)
  10
  0
)

(/ 1 1.6180)