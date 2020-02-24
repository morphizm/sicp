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


(define x (list (list 1 2) (list 3 4)))

; (reverse (list 23 72 149 34))
; (list 34 149 72 23)

(define (deep-reverse items)
    (define (last elems)
      (cond ((pair? (car elems)) (reverse (car elems)))
        (else (cdr elems))
      )
    )
  (define (iter elems acc)

    (if (null? elems)
      acc
      (iter (cdr elems) (cons (last elems) acc))
    )
  )
  (iter items list)
)


(reverse x)
(deep-reverse x)