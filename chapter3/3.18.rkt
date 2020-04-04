#lang racket/base

(require rackunit)
(require "mutable-pairs.rkt")


(define (has-cycle? pairs)
  (define passed-pairs '())
  (define (iter items)
    (if (null? items)
      #f
        (if (has-pair? passed-pairs items)
          #t
          (begin
            (set! passed-pairs (cons items passed-pairs))
            (iter (cdr items))))))
  (iter pairs))

(define pairs (cons 1 (cons 2 (cons 3 '()))))
(define pairs2 (cons (cons 1 1) (cons 2 (cons 3 '()))))

(define second (cdr pairs))
(define third (cdr (cdr pairs)))
(define last (cdr (cdr (cdr pairs))))

; no
(set-car! pairs third)
(check-equal? (has-cycle? pairs) #f)

; no
(set-car! pairs second)
(set-car! second third)
(check-equal? (has-cycle? pairs) #f)

; no
(check-equal? (has-cycle? pairs2) #f)

(define z (make-cycle (list 'a 'b 'c)))
; yes
(check-equal? (has-cycle? z) #t)
