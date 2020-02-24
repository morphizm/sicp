#lang racket/base

(require "func.rkt")
(require rackunit)

(equal? '(this is a list) '(this is a list))

(equal? '(this is a list) '(this (is a) list))

(define (equal items1 items2)
  (equal?
    (enumerate-tree items1) (enumerate-tree items2)
  )
)


(equal '(this is a list) '(this (is a) list))

(equal? 'a '(a))