#lang racket/base
(require "./streams.rkt")

(define s1 (cons-stream 1 (cons-stream 2 (cons-stream 3 (cons-stream 4 the-empty-stream)))))
(define s2 (cons-stream 40 (cons-stream 50 (cons-stream 60 the-empty-stream))))

(define (merge s1 s2)
  (cond [(stream-null? s1) s2]
        [(stream-null? s2) s1]
        [else
          (let [(s1car (stream-car s1))
                (s2car (stream-car s2))]
            (cond [(< s1car s2car) (cons-stream s1car (merge (stream-cdr s1) s2))]
                  [(> s1car s2car) (cons-stream s2car (merge s1 (stream-cdr s2)))]
                  [else
                    (cons-stream s1car
                                 (merge (stream-cdr s1)
                                        (stream-cdr s2)))]))]))

(display-stream (merge s1 s2))
(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream 5)))))
(display-stream S)