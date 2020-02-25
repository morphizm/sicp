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


(define (union-set set1 set2)
  (cond
    ((null? set1) set2)
    ((null? set2) set1)
    ((= (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) (cdr set2))))
    ((< (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) set2)))
    ((> (car set1) (car set2)) (cons (car set2) (union-set set1 (cdr set2))))
  )
)


(define x (list 1 2 3))
(define y (list 4 5 6))

(element-of-set? 1 x)
(element-of-set? 10 x)
(intersection-set x x)
(union-set x x)
(union-set x y)
(union-set (list) (list))
(union-set (list 1 3 5) (list 2 4 6))
