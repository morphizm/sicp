#lang racket/base
(require "./streams.rkt")

; 1 2 -5 -6 -7 4 4 ...
; 0 0 -1 0   0 1 0 ...

(define (sign-change-detector new-value old-value)
  (displayln new-value)
  (displayln old-value))
(define (make-zero-crossings input-stream last-value)
  (if (stream-null? input-stream)
      the-empty-stream
  (cons-stream
    (sign-change-detector (stream-car input-stream) last-value)
    (make-zero-crossings (stream-cdr input-stream)
                          (stream-car input-stream)))))

(define integers (stream-enumerate-interval 1 10))
;(display-stream (make-zero-crossings integers))

(define sense-data integers)
(define zero-crossings
  (stream-map sign-change-detector sense-data (stream-cdr sense-data)))


