#lang racket/base
(require "./streams.rkt")

(define s1 (cons-stream 1 (cons-stream 2 (cons-stream 3 (cons-stream 4 the-empty-stream)))))
(define s2 (cons-stream 40 (cons-stream 50 (cons-stream 60 the-empty-stream))))

(define make-series (lambda (const arg) (cons-stream const arg)))
(define (integrate-series s)
  (define (iter seq i)
    (if (stream-null? seq)
        the-empty-stream
        (cons-stream (make-series (/ 1 i) (stream-car seq)) (iter (stream-cdr seq) (+ i 1)))))
  (iter s 1))

(define power-series (cons-stream 1 (cons-stream 'a0 (cons-stream 'a1 (cons-stream 'a2 (cons-stream 'a3 the-empty-stream))))))
(integrate-series power-series)
(display-stream (integrate-series power-series))

(define exp-series (cons-stream 1 (integrate-series exp-series)))

(define cosine-stream (cons-stream 1 (integrate-series )))
(define sine-series (cons-stream 0 ()))