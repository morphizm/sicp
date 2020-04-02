#lang racket/base

(require rackunit)
(require "mutable-pairs.rkt")

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define pairs (cons 1 (cons 2 (cons 3 '()))))

; 3
(count-pairs pairs)

; Infinity
; (define infinity-pairs (make-cycle pairs))
; (count-pairs infinity-pairs)

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
