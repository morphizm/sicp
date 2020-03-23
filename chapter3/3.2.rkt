#lang racket/base

(require rackunit)

(define (make-monitored proc)
  (let ([state 0]
        [defaultState 0])
    (define (mf type)
      (cond ((eq? type 'how-many-calls?) state)
            ((eq? type 'reset-count) (set! state defaultState))
            (else (begin (set! state (+ state 1)) (proc type)))))
    mf))


(define s (make-monitored sqrt))
(define ss (make-monitored sqrt))

(check-equal? (s 100) 10)
(check-equal? (s 'how-many-calls?) 1)
(check-equal? (ss 'how-many-calls?) 0)
(s 'reset-count)
(check-equal? (s 'how-many-calls?) 0)
(check-equal? (ss 'how-many-calls?) 0)

(ss 100)
(s 100)
(s 100)
(s 100)
(check-equal? (s 'how-many-calls?) 3)
(check-equal? (ss 'how-many-calls?) 1)

