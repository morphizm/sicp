#lang racket/base

(require rackunit)
(provide cons set-car! set-cdr! car cdr list)

(define cons mcons)
(define car mcar)
(define cdr mcdr)
(define set-car! set-mcar!)
(define set-cdr! set-mcdr!)

(define (list first . rest)
  (if (null? rest)
    (cons first '())
    (cons first (apply list rest))
  )
)
; (define (set-car! pair new-car)
  ; (set! pair (cons new-car (cdr pair)))
  ; pair)

; (define (set-cdr! pair new-cdr)
  ; (set! pair (cons (car pair) new-cdr))
  ; pair)



; Example working
; (define x (cons 1 2))
; x
; (set-car! x 3)
; x; 3 2
; (set-cdr! x 22)
; x; 3 22