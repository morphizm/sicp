#lang racket/base


(require racket)
(require rackunit)

(provide nil ok? true?)
(define nil '())

(define (ok? sym) (eq? sym 'ok))

(define (true? exp) (eq? exp #t))


(check-equal? (true? #t) #t)
(check-equal? (ok? 'ok) #t)
