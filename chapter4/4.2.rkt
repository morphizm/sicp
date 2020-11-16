#lang racket/base

(require "./func.rkt")
(require "./lisp.rkt")


(define quote1 (list 'quote 5))
(eval quote1 nil)

(define def (list 'define 'x 3))

(eval def nil)


(define (lisp)
  (define operations (list))
  (define (push! items item)
    (define (iter coll acc)
      (if (null? coll)
        (set! operations (cons item acc))
        (iter (cdr coll) (cons (car coll) acc))))
    (iter items nil))

  (define (eval exp env))

  (define (put operation type func))

  (lambda (proc)
    (cond
      [(eq? proc 'eval) eval])))

