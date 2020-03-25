#lang racket/base

(require rackunit)
(require "math.rkt")

; rand-update x
; ax + b mod m
(define random-init 100)
(define (rand-update x)
  (+ (* 32 x) (remainder 63 15)))

(define rand
  (let ([x random-init])
    (lambda ()
      (set! x (rand-update x))
      x)))

; Monte Carlo simulation
; 6 / (pi * pi)

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond
      ((= trials-remaining 0) (/ trials-passed trials))
      ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
      (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


