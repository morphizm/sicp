#lang racket/base
(require "./streams.rkt")

(define integers (stream-enumerate-interval 1 100))
(stream-ref integers 3)

(pairs integers integers)
(display-stream (pairs integers integers))

; 1, 100 ---- ~200
; 99, 100 ---- ~5000
; 100, 100 ---- ~5001
