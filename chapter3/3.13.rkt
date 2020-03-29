#lang racket/base

(require rackunit)
(require "mutable-pairs.rkt")

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)  
  x)

(define x (cons 1 2))
(define z (make-cycle (list 'a 'b 'c)))
z
(cdr (cdr (cdr (cdr (cdr (cdr z))))))
