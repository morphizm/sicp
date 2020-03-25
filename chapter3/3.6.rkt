#lang racket/base

(require rackunit)
(require "math.rkt")

(define (rand-update x)
  (+ (* 3 x) (remainder 66 15)))

(define random-init 100)
(define rand
  (let ([x random-init])
    (lambda (type)
      (cond
        ((eq? 'generate type) (begin (set! x (rand-update x)) x))
        ((eq? 'reset type) (lambda (new-x) (begin (set! x new-x) x)))
        (else (error "Unknown rand type -- RAND" type))))))

(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)

((rand 'reset) 20)
(rand 'generate)
(rand 'generate)
((rand 'reset) 100)
(rand 'generate)
