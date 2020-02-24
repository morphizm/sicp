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

(define x (cons (list 1 2) (list 3 4)))
(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op init (map (lambda (x) (car x)) seqs))
          (accumulate-n op init (map (lambda (x) (cdr x)) seqs))
    )
  )
)

(accumulate-n + 0 s)