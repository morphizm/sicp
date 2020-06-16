#lang racket/base
(require "./streams.rkt")

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(define (stream-limit s tolerance)
  (define (iter seq)
    (let [(current (stream-car seq))]
         [(next (stream-car (stream-cdr seq)))]
      (if (< (- next current) tolerance)
          next
          (iter (stream-cdr seq)))))
  (iter s))
