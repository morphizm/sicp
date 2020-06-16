#lang racket/base
(require "./streams.rkt")

;(define (s) (cons-stream 1 (add-streams (s) (s))))
; 2 2 2 2 2 ....
;(stream-ref (s) 2)

(define s1 (cons-stream 1 (cons-stream 2 (cons-stream 3 the-empty-stream))))
(define s2 (cons-stream 40 (cons-stream 50 (cons-stream 60 the-empty-stream))))

(display-stream (add-streams s1 s2))
