#lang racket/base
(require "./streams.rkt")

(define s1 (cons-stream 1 (cons-stream 2 (cons-stream 3 (cons-stream 4 the-empty-stream)))))
(define s2 (cons-stream 40 (cons-stream 50 (cons-stream 60 the-empty-stream))))

(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))
(define (expand-o num den radix i)
  (if (> i 10) the-empty-stream
  (cons-stream
    (quotient (* num radix) den)
    (expand-o (remainder (* num radix) den) den radix (+ i 1)))))

;(stream-ref (expand 1 7 10) 1); 1 4 2 8 5 7
;(stream-ref (expand 3 8 10) 1); 3 7 5 0 0 0
(display-stream (expand-o 1 7 10 0))
(display-stream (expand-o 3 8 10 0))