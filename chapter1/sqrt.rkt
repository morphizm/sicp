#lang racket/base

(require rackunit)

; (define (good-enough? guess x)
  ; (< (abs (- (square guess) x)) 0.001)
; )
; Точность улучшилась при сравнение прошлого приближения и текущего

(define (square x) (* x x))

(define (sqrt x)
  (define (sqrt-iter last-guess guess)
    (if (good-enough? last-guess guess)
      guess
      (sqrt-iter guess (improve guess))
    )
  )
  (define (improve guess)
    (average guess (/ x guess))
  )
  (define (average a b)
    (/ (+ a b) 2)
  )
  (define (good-enough? last-guess guess)
    (< (abs (- last-guess guess)) 0.001)
  )
  (sqrt-iter 0 1.0)
)

(sqrt 137)

(sqrt (+ (sqrt 2) (sqrt 3)))

; (check-equal? (sqrt 4) 2)
