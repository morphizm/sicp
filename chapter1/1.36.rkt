#lang racket/base

(require rackunit)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess i)
    (display guess)
    (newline)
    (display i)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next (+ i 1))
      )
    )
  )
  (try first-guess 0)
)

; (fixed-point cos 1.0)

(define (x-x-1000)
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 2)
)

(x-x-1000)

(define (g x) 
  (define (average x y) (/ (+ x y) 2)) 
  (average x (/ (log 1000) (log x))))
(define (f x) (/ (log 1000) (log x)))
(fixed-point f 2.0)
(fixed-point g 2.0) 