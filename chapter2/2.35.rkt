#lang racket/base

(require rackunit)
(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
      (accumulate op initial (cdr sequence)))
  )
)

(define (map proc sequence)
  (accumulate (lambda (x acc) (cons (proc x) acc))
    nil sequence
  )
)

(define (append seq1 seq2)
  (accumulate cons seq2 seq1)
)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
    ((not (pair? tree)) (list tree))
    (else (append (enumerate-tree (car tree))
                  (enumerate-tree (cdr tree))))
  )
)

; (define (count-leaves t)
;   (accumulate
;     (lambda (x acc) (+ 1 acc))
;     0
;     (enumerate-tree t)
;   )
; )

(define (count-leaves t)
  (accumulate
    +
    0
    (map (lambda (x) (if (pair? x) (count-leaves x) 1)) t)
  )
)

(define x (cons (list 1 2) (list 3 4)))

(count-leaves (list x x))