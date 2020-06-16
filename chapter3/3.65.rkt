#lang racket/base
(require "./streams.rkt")

(define (ln-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-summands (+ n 1)))))
(define ln-stream
  (scale-stream (partial-sums (ln-summands 1)) 2))
