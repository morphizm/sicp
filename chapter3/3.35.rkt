#lang racket/base

(require "./limitations.rkt")

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if [< (get-value b) 0]
            (error "Square less zero -- SQUARER" (get-value b))
            [begin
              (set-value! a (sqrt (get-value b)) me)
              (multiplier a a b)])
        [multiplier a a b]))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me))
  (define (me request)
    (cond [(eq? request 'I-have-a-value)
           (process-new-value)]
          [(eq? request 'I-lost-my-value)
           (process-forget-value)]))
  (connect a me)
  (connect b me)
  me)

(define A (make-connector))
(define B (make-connector))
(probe "A" A)
(probe "B" B)
(squarer A B)

(set-value! A 10 'user)
(forget-value! A 'user)
(set-value! B 100 'user)