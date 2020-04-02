#lang racket/base

(require rackunit)
(require "mutable-pairs.rkt")

(define (count-pairs x)
  (define pairs '())
  (define (iter elems acc)
    (if (null? elems)
      acc
      (let ([first (car elems)]
            [rest (cdr elems)])
        (if (not (pair? first))
            (iter rest acc)
            (if (has-pair? pairs first)
                (iter rest acc)
                (begin
                  (set! pairs (cons first pairs))
                  (iter (cons (car first) (cons (cdr first) rest)) (+ acc 1)))
            )))))
  (iter x 0)
)


(define pairs (cons 1 (cons 2 (cons 3 '()))))
(define pairs2 (cons (cons 1 1) (cons 2 (cons 3 '()))))

; 3
(count-pairs pairs)

(define second (cdr pairs))
(define third (cdr (cdr pairs)))
(define last (cdr (cdr (cdr pairs))))

second
third
last

; 4
(set-car! pairs third)
(count-pairs pairs)

; 7
(set-car! pairs second)
(set-car! second third)
(count-pairs pairs)

(count-pairs pairs2)