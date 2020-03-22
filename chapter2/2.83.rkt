#lang racket/base

(require "func.rkt")
(require "complex.rkt")
(require rackunit)

; integer 
; rational
; real
; complex

(define (integer->rational num)
  (make-rat num 1))

(define (rational->real num)
;;;;;;??????)
  (make-real (/ (numer num) (denom num))))

(define (real->complex num)
  (make-from-real-imag-rectangular num 0))

  ;
(put 'raise '(scheme-number rational) integer->rational)
(put 'raise '(rational real) rational->real)
(put 'rause '(real complex) real->complex)

(define (raise x) (apply-generic 'raise x))