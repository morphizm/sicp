#lang racket/base

(require "func.rkt")
(require rackunit)

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

(define z
  (cons 'complex (cons 'rectangular (cons 3 4)))
)

(magnitude z)
