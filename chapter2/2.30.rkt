#lang racket/base

(define nil '())

(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items)) (map proc (cdr items)))
  )
)

(define (square-tree tree)
  (map
    (lambda (sub-tree)
      (if (pair? sub-tree)
          (square-tree sub-tree)
          (* sub-tree sub-tree)
      )
    )
    tree
  )
)

(define (square-tree-rec tree)
  (cond ((null? tree) nil)
    ((not (pair? tree)) (* tree tree))
    (else (cons (square-tree-rec (car tree)) (square-tree-rec (cdr tree))))
  )
)

(square-tree-rec
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)
  )
)
(square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)
  )
)