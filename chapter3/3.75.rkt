#lang racket/base
(require "./streams.rkt")

(define (make-zero-crossings input-stream last-value acc)
  (let [(avpt (avarage acc))]
    (cons-stream (sign-change-detector avpt last-value)
                  (make-zero-crossings (stream-cdr input-stream)
                                        avpt
                                        (cons-stream (strea-car input-stream) acc)))))
