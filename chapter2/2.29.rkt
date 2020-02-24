#lang racket/base

(define (make-mobile left right)
  (list left right)
)
; изменится структура поменяем немного селекторы
(define (left-branch mobile)
  (car mobile)
)
(define (right-branch mobile)
  (car (cdr mobile))
)

(define x (make-mobile 1 2))
(right-branch x)
(left-branch x)

(define (make-branch length structure)
  (list length structure)
)

(define (branch-length branch)
  (car branch)
)
(define (branch-structure branch)
  (car (cdr branch))
)

(define (get-weight-structure structure cb)
  (cond ((pair? structure) (cb structure))
    (else structure)
  )
)

(define (total-weight mobile)
  (let ([structure-left (branch-structure (left-branch mobile))] 
        [structure-right (branch-structure (right-branch mobile))])
    (+ (get-weight-structure structure-left total-weight)
      (get-weight-structure structure-right total-weight)
    )
  )
)

(define br (make-branch 2 2))
(define mb (make-mobile br br))

(define br2 (make-branch 2 mb))
(define mb2 (make-mobile br2 br2))
(total-weight mb)

(define (get-moment weight length)
  (* weight length)
)

(define (mobile-balanced? mobile)
  (let ([left-branch (left-branch mobile)]
        [right-branch (right-branch mobile)])
    (cond ((= 
            (get-moment (get-weight-structure (branch-structure left-branch) total-weight) 
                        (branch-length left-branch))
            (get-moment (get-weight-structure (branch-structure right-branch) total-weight) 
                        (branch-length right-branch))
          ) #t)
      (else #f)
    )
  )
)

(mobile-balanced? mb)
(mobile-balanced? mb2)

(define br3 (make-branch 3 2))
(define mb3 (make-mobile br br3))
(mobile-balanced? mb3)
