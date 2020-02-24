#lang racket/base

(require rackunit)

(define (reverse items)
  (define (iter elems acc)
    (if (null? elems)
      acc
      (iter (cdr elems) (cons (car elems) acc))
    )
  )
  (iter items list)
)

(define (parity x) (remainder x 2))

(define (same-parity x . rest)
  (define x-parity (parity x))
  (define (iter items acc)
      (cond
          ((null? items) (reverse acc))
          ((= x-parity (parity (car items)))
            (iter (cdr items) (cons (car items) acc))
          )
          (else (iter (cdr items) acc))
      )
  )
  (iter rest (list x))
)

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
