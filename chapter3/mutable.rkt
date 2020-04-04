#lang racket/base

(require rackunit)
(provide cons set-car! set-cdr! car cdr list pair? last-pair make-cycle
         has-pair?
)

(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Unknown opertaion -- CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)

(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)  
  x)

(define (has-pair? pairs pair)
  (if (null? pairs)
      #f
      (let ([current (car pairs)])
        (if (eq? current pair)
            #t
            (has-pair? (cdr pairs) pair)))))
