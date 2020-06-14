#lang racket/base
(require "./streams.rkt")

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc (map stream-cdr argstreams))))))

(define s1 (cons-stream 1 (cons-stream 2 (cons-stream 3 the-empty-stream))))
(define s2 (cons-stream 40 (cons-stream 50 (cons-stream 60 the-empty-stream))))


(define result
  (stream-map + s1 s2))

(display-stream result) ; 41 52 63

(define s4 (cons-stream 4 (cons-stream 5 (cons-stream 6 the-empty-stream))))

(define result2
  (stream-map (lambda (x y) (+ x (* 2 y)))
              s1
              s4))

(display-stream result2) ; 9 12 15
