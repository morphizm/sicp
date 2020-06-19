#lang racket/base
(require "./streams.rkt")

; +    v    -
; i -> R -> C
; v = v0 + 1/C * integral idt + Ri

(define (integral i dt)
  (* i dt))

(define (RC R C dt)
  (lambda (i-stream v0)
    (stream-map (lambda (i)
                  (+ v0 (* R i) (* (/ 1 C) (integral i dt))))
                i-stream)))



(define stream
  (cons-stream 0 (cons-stream 1 (cons-stream 1.2 (cons-stream 1.3 (cons-stream 1.5 (cons-stream 2.3 the-empty-stream)))))))
(define RC1 (RC 5 1 0.5))

(define result (RC1 stream 4))
(display-stream result)
