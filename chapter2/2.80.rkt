#lang racket/base

(require "func.rkt")
(require "complex.rkt")
(require rackunit)

; (define (apply-generic x q) 0)
(define (=zero? num)
  (apply-generic '=zero? num))


(define (scheme-num-zero? n) (= n 0)

(put '=zero? '(scheme-number scheme-number) scheme-num-zero?)

(define (rat-zero? n)
  (or (= (numer n) 0) (= (denom n) 0)))

(put '=zero? '(rational rational) rat-zero?)

(define (complex-zero? n)
  (and (= (real-part n) 0) (= (imag-part n) 0)))

(put '=zero? '(complex complex) complex-zero?)
