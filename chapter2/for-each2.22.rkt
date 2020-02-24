#lang racket/base

(require rackunit)
(define nil '())

(define (for-each proc items)
  (proc (car items))
  (unless (null? (cdr items))
    (for-each proc (cdr items))
  )
)

(for-each (lambda (x) (* x x)) (list 1 2 3 4))

(for-each (lambda (x) (newline) (display x))
  (list 57 321 88)
)