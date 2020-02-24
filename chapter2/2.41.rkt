#lang racket/base

(require "func.rkt")
(require rackunit)

(define (sum-of-three-nums n s)
  (define nums (accumulate append
    nil
    (map (lambda (i)
          (map (lambda (j)
                (map (lambda (k) (list i j k)) (enumerate-interval 1 n)))
                (enumerate-interval 1 n)))
          (enumerate-interval 1 n))))
  (define flat-nums (flatmap (lambda (x) x) nums))
  (filter (lambda (n) (= (sum n) s)) flat-nums))


(sum-of-three-nums 5 8)
(sum-of-three-nums 10 5)
; (sum-of-three-nums 20 20)
