#lang racket/base
(require "./streams.rkt")

(define s1 (cons-stream 1 (cons-stream 2 (cons-stream 3 (cons-stream 4 the-empty-stream)))))
(define s2 (cons-stream 40 (cons-stream 50 (cons-stream 60 the-empty-stream))))

; s0, s0+s1, s0+s1+s2
(define (partial-sums s)
  (define (iter seq prev acc)
    (cond [(stream-null? seq) acc]
          [else (iter (stream-cdr seq) (+ prev (stream-car seq)) (cons-stream (+ prev (stream-car seq)) acc))]))
  (iter s 0 '(0)))
; partial-sums integers == 1 3 6 10 15 ...

(display-stream (partial-sums s1))