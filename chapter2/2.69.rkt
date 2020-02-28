#lang racket/base

(require "func.rkt")
(require "haffman.rkt")
(require rackunit)

(define (generate-huffman-tree pairs)
  (succesive-merge (make-leaf-set pairs)))

(define (succesive-merge structure)
  (define (merge-1 pairs acc)
    (if (= (length pairs) 1)
      (make-code-tree (car pairs) acc)
      (merge-1 (cdr pairs)
               (make-code-tree (car pairs) acc))))
  (merge-1 (cdr structure) (car structure)))

(define mes '(A D A B B C A)) ; '(A D A B B C A)
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define sample-pairs '((A 4) (B 2) (D 1) (C 1)))
; sample-tree
; (make-leaf-set sample-pairs)
; (generate-huffman-tree sample-pairs)

; (make-code-tree (make-leaf 'A 4) (make-leaf 'A 4))

(check-equal? (generate-huffman-tree sample-pairs) sample-tree)