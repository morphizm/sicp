#lang racket/base

(define (sqrt-iter last-guess guess x)
  (if (good-enough? last-guess guess)
    guess
    (sqrt-iter guess (improve guess x) x)
  )
)

(define (improve guess x)
  (average guess (/ x guess))
)

(define (average x y)
  (/ (+ x y) 2)
)

(define (good-enough? last-guess guess)
  (< (abs (- last-guess guess)) 0.001)
)

; (define (good-enough? guess x)
  ; (< (abs (- (square guess) x)) 0.001)
; )
; Точность улучшилась при сравнение прошлого приближения и текущего

; (define (abs num)
;   (cond ((<= num 0) (- num))
;     (num)
;   )
; )

(define (square x) (* x x))


(define (sqrt num)
  (sqrt-iter 0 1.0 num)
)

(sqrt 137)

(sqrt (+ (sqrt 2) (sqrt 3)))
