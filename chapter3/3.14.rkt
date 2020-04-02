#lang racket/base

(require rackunit)
(require "mutable-pairs.rkt")

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ([temp (cdr x)])
          ; (displayln x)
          ; (displayln y)
          ; (displayln temp)
          (set-cdr! x y)
          (loop temp x)
          )))
  (loop x '()))

; (mystery (list 1 2 3 4 5 6 7 8))

(define v (list 'a 'b 'c 'd))
v
(define w (mystery v))
v
w