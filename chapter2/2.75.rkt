#lang racket/base

(require "func.rkt")
(require rackunit)

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond
      ((eq? op 'real-part) x)
      ((eq? op 'imag-part) y)
      ((eq? op 'magnitude)
        (sqrt (+ (square x) (square y))))
      ((eq? op 'angle) (atan y x))
      (else
        (error "Unknown operation --MAKE-FROM-REAL-IMAG" op))
    )
  )
  dispatch
)

(define (apply-generic op arg) (arg op))

(define (make-from-mag-ang r a)
    (define (dispatch op)
    (cond
      ((eq? op 'real-part) (* x (cos r)))
      ((eq? op 'imag-part) (* x (sin r)))
      ((eq? op 'magnitude) x)
      ((eq? op 'angle) y)
      (else
        (error "Unknown operation --MAKE-FROM-REAL-IMAG" op))
    )
  )
  dispatch
)