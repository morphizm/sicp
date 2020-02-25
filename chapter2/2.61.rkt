#lang racket/base

(require "func.rkt")
(require rackunit)

(define (element-of-set? x set)
  (cond
    ((null? set) #f)
    ((= x (car set)) #t)
    ((< x (car set)) #f)
    (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ([x1 (car set1)] [x2 (car set2)])
      (cond
        ((= x1 x2)
          (cons x1 (intersection-set (cdr set1) (cdr set2))))
        ((< x1 x2)
          (intersection-set (cdr set1) set2))
        ((> x1 x2)
          (intersection-set set1 (cdr set2)))))))


(define (adjoin-set x set)
  (cond
    ((null? set) (cons x '()))
    ((= x (car set)) set)
    ((< x (car set)) (cons x set))
    (else (cons (car set) (adjoin-set x (cdr set))))
  )
)

(define x (list 1 2 3))
(define y (list 4 5 6 9))

(element-of-set? 1 x)
(element-of-set? 10 x)
(adjoin-set 3 x)
(adjoin-set 4 x)
(adjoin-set 0 x)
(adjoin-set 8 y)
(intersection-set x x)
; (union-set x x)
; (union-set x y)
