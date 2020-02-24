#lang racket/base

(require rackunit)
(define nil '())

(define (square-list items)
  (if (null? items)
    nil
    (cons (* (car items) (car items)) (square-list (cdr items)))
  )
)

(square-list (list 1 2 3 4))

(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items)) (map proc (cdr items)))
  )
)

(define (square-list-map items)
  (map (lambda (x) (* x x)) items)
)
(square-list-map (list 1 2 3 4))
