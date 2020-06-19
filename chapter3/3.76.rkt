#lang racket/base
(require "./streams.rkt")

(define (make-zero-crossings input-stream last-value)
  (cons-stream
    (sign-change-detector (stream-car input-stream) last-value)
    (make-zero-crossings (stream-cdr input-stream)
                          (stream-car input-stream)))))

(define (smooth stream last-value acc)
    (let [(avpt (avarage acc))]
    (cons-stream avpt
                  (smooth (stream-cdr input-stream)
                                        avpt
                                        (cons-stream (strea-car input-stream) acc)))))


