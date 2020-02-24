#lang racket/base

(require rackunit)
(define nil '())

(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items)) (map proc (cdr items)))
  )
)

(map (lambda (x) (* x x)) (list 1 2 3 4))