#lang racket/base

(require "func.rkt")
(require rackunit)

(define (entry tree) (car tree))
(define (right-branch tree) (caddr tree))
(define (left-branch tree) (cadr tree))
(define (make-tree entry left right)
  (list entry left right))

; append пройдется по листу
(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

; растет медленней
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree) result-list)
                    )
      )
    )
  )
  (copy-to-list tree '())
)

(define example-tree
  (make-tree 7
             (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '()))
             (make-tree 9 '() (make-tree 11 '() '()))
  )
)

(define example-tree-2
  (make-tree 3
             (make-tree 1 '() '())
             (make-tree 7 (make-tree 5 '() '()) (make-tree 9 '() (make-tree 11 '() '())))
  )
)

(define example-tree-3
  (make-tree 5
             (make-tree 3 (make-tree 1 '() '()) '())
             (make-tree 9 (make-tree 7 '() '()) (make-tree 11 '() '()))
  )
)

(tree->list-1 example-tree)
(tree->list-2 example-tree)

(tree->list-1 example-tree-2)
(tree->list-2 example-tree-2)

(tree->list-1 example-tree-3)
(tree->list-2 example-tree-3)