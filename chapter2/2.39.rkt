#lang racket/base

(define nil '())

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest)) (cdr rest))
    )
  )
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
      (fold-right op initial (cdr sequence)))
  )
)

(define (append seq1 seq2)
  (fold-right cons seq2 seq1)
)

(define (reverse sequence)
  (fold-right (lambda (x acc) (append acc (list x))) nil sequence)
)

(define (reverse2 sequence)
  (fold-left (lambda (x acc) (cons acc x)) nil sequence)
)

(reverse (list 1 2 3))
(reverse2 (list 1 2 3))
