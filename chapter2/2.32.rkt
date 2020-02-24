#lang racket/base

(define nil '())

(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items)) (map proc (cdr items)))
  )
)

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))
  )
)

(define (subsets s)
  (if (null? s)
    (list nil)
    (let ([rest (subsets (cdr s))])
      (append rest (map (lambda (num) (cons (car s) num)) rest)))
  )
)

(subsets (list 1 2 3))