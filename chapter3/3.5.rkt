#lang racket/base

(require rackunit)
(require "math.rkt")

; P x y
; r = 3
; (5, 7)
; (x-5)**2 + (y-7)**2 <= 3**2

(define (estimate-integral P x1 x2 y1 y2 quantity-count)
  (define square-rectangle (* (- y2 y1) (- x2 x1)))
  ; (displayln square-rectangle)
  (define (iter trials acc)
    (let ([x (random-in-range x1 x2)]
          [y (random-in-range y1 y2)])
      (cond
        ((>= trials quantity-count) (/ (* acc square-rectangle) (+ 0.0 quantity-count)))
        ((P x y) (iter (+ trials 1) (+ acc 1)))
        (else (iter (+ trials 1) acc))
      )
    )
  )
  (iter 0 0)
)

(define (random-in-range low high)
  (let ([range (- high low)])
    (+ low (random range))))



(define (predicat x y)
  (>= (square 3) 
      (+ (square (- x 5)) (square (- y 7)))))


(estimate-integral predicat 2 8 4 10 100)

(define (pred-pi x y)
  (>= (square 1)
      (+ (square (- x 5)) (square (- y 7)))))

(estimate-integral pred-pi 4 6 6 8 1000)
