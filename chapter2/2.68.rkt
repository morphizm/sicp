#lang racket/base

(require "func.rkt")
(require "haffman.rkt")
(require rackunit)


(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  ; (error "Can not encode a symbol" symbol)
  (define (encode-1 tree acc)
    (cond ((null? tree) '())
          ((leaf? tree) (if (eq? symbol (symbol-leaf tree)) (reverse acc) #f))
          (else
            (let ([left (left-branch tree)]
                  [right (right-branch tree)])
              (let ([left-result (encode-1 left (cons 0 acc))]
                    [right-result (encode-1 right (cons 1 acc))])
                (if left-result left-result right-result))))))
  (encode-1 tree '()))

(define mes '(A D A B B C A)) ; '(A D A B B C A)
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; (decode sample-message sample-tree)

; sample-tree

; (encode '(S) sample-tree)
; (encode mes sample-tree)


(check-equal? 
  (encode mes sample-tree)
  sample-message)