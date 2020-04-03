#lang racket/base

(require rackunit)
(require "mutable-pairs.rkt")


(define (count-pairs x)
  (define pairs '())
  (define (iter elems)
    (if (not (pair? elems))
      0
      (if (has-pair? pairs elems)
        (+ (iter (car elems))
           (iter (cdr elems))
          0)
        (begin 
          (set! pairs (cons elems pairs))
          (+ (iter (car elems))
             (iter (cdr elems))
            1)))))
  (iter x))

(define pairs (cons 1 (cons 2 (cons 3 '()))))
(define pairs2 (cons (cons 1 1) (cons 2 (cons 3 '()))))

; 3
(count-pairs pairs)

(define second (cdr pairs))
(define third (cdr (cdr pairs)))
(define last (cdr (cdr (cdr pairs))))

; second
; third
; last

; 4 ---- 3!
(set-car! pairs third)
(count-pairs pairs)

; 7 --- 3!
(set-car! pairs second)
(set-car! second third)
(count-pairs pairs)

; 4
(count-pairs pairs2)
