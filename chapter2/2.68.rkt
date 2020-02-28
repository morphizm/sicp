#lang racket/base

(require "func.rkt")
(require "haffman.rkt")
(require rackunit)

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  ; (error "Can not encode a symbol" symbol)
  (define (encode-1 current-tree acc)
    (if (null? current-tree)
        '()
        (let ([current-symbol (symbol-leaf current-tree)])
          (if (eq? current-symbol symbol)
              acc
              
          )
        )
    )
  )
  (encode-1 tree '())
)

(define mes '(A D A B B C A))

sample-tree

(encode mes sample-tree)
