#lang racket/base

(define balance 100)

; Petr
(set! balance (+ balance 10))
; Pavel
(set! balance (- balance 20))
; Mary
(set! balance (- balance (/ balance 2)))

; Posible values is
; 40 35 50
