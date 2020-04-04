#lang racket/base

(require rackunit)
(require "mutable-pairs.rkt")


(define (has-cycle? pairs)
  (define (new-current-item items count)
    (if (odd? count)
      (cdr items)
      items))
  (define (iter items current-item count)
    (if (null? items)
      #f
        (if (eq? items current-item)
          #t
          (begin
            (iter (cdr items) (new-current-item current-item count) (+ 1 count))))))
  (iter (cdr pairs) pairs 0))

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
(check-equal? (has-cycle? (make-cycle (list 'a))) #t)
