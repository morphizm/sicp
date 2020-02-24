#lang racket/base

(require rackunit)
(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
      (accumulate op initial (cdr sequence)))
  )
)

(define (map proc sequence)
  (accumulate (lambda (x acc) (cons (proc x) acc))
    nil sequence
  )
)

(map (lambda (x) (* x x)) (list 1 2 3 4))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1)
)

(define x (list 1 2 3 4))
(define y (list 9 8 7 6))
(append x y)

(define (length sequence)
  (accumulate (lambda (x acc) (+ 1 acc)) 0 sequence))

(length x)