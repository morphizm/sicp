#lang racket/base

(require rackunit)

(define (square x)
  (* x x))

(check-equal? (square 4) 16)
