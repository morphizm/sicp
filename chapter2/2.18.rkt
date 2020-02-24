#lang racket/base

(require rackunit)
(define nil '())

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))
  )
)

(define (reverse items)
  (define (iter elems acc)
    (if (null? elems)
      acc
      (iter (cdr elems) (cons (car elems) acc))
    )
  )
  (iter items list)
)


(reverse (list 23 72 149 34))
(list 34 149 72 23)