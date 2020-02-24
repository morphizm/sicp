#lang racket/base

(require "func.rkt")
(require rackunit)

(define empty-board nil)

(define (safe? k positions)
  #t
)

(define (adjoin-position row k queens) 
 0 
)

(define (queens board-size)
  (define (queens-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap 
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                    (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)
            )
          )
          (queens-cols (- k 1))
        )
      )
    )
  )
  (queens-cols board-size)
)




(queens 8)
