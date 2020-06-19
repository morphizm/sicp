#lang racket/base
(require "./streams.rkt")

(define integers (stream-enumerate-interval 1 100))
(stream-ref integers 3)

(define (pairs s t)
  (if (stream-null? s)
    the-empty-stream
  (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                t)
    (pairs (stream-cdr s) (stream-cdr t)))))

(pairs integers integers)
(display-stream (pairs integers integers))
