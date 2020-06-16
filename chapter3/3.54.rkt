#lang racket/base
(require "./streams.rkt")

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define s1 (cons-stream 1 (cons-stream 2 (cons-stream 3 the-empty-stream))))
(define s2 (cons-stream 40 (cons-stream 50 (cons-stream 60 the-empty-stream))))

(define factorials (cons-stream 1 (mul-streams (stream-cdr integers) factorials)))

; (display-stream (mul-streams s1 s2))
(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers))
; 1 2 3 4 5 6