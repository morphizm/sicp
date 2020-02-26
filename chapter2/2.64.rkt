#lang racket/base

(require "func.rkt")
(require rackunit)

(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ([left-size (quotient (- n 1) 2)])
      (let ([left-result (partial-tree elts left-size)])
        (let ([left-tree (car left-result)]
              [non-left-elts (cdr left-result)]
              [right-size (- n (+ left-size 1))])
              ; (displayln left-result)
          (let ([this-entry (car non-left-elts)]
                [right-result (partial-tree (cdr non-left-elts) right-size)])
            (let ([right-tree (car right-result)]
                  [remaining-elts (cdr right-result)])
                  ; (displayln this-entry)
                (cons (make-tree this-entry left-tree right-tree) remaining-elts)
            )
          )
        )
      )
    )
  )
)
; Вычисляем длины правого и левого поддеревьев
; Вычисляем правые и левые дерерья рекурсивно
; 
; 

(list->tree (list 1 2 5 4))
(list->tree (list 1 3 5 7 9 11))
