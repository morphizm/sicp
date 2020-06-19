#lang racket/base
(require racket)
;(require "./streams.rkt")

(define (cons-stream a b) (cons a (delay b)))
(define (stream-cdr stream) (force (cdr stream)))
(define (stream-car stream) (car stream))

;(define int (cons-stream 1 (stream-cdr int)))
(define int (cons-stream 1 (delay int)))
