#lang racket/base

(define (improve-sqrt-cube x y)
  (/ (+ (/ x (* y y)) (* 2 y)) 3)
)

(define (sqrt-cube-iter  guess x)
  (if (good-enough? guess x)
    (improve-sqrt-cube x guess)
    (sqrt-cube-iter (improve guess x) x)
  )
)

(define (improve guess x)
  (average-cube (* 2 guess) (/ x (* guess guess)))
)

(define (average-cube x y)
  (/ (+ x y) 3)
)

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001)
)

; (define (abs num)
;   (cond ((<= num 0) (- num))
;     (num)
;   )
; )

(define (cube x) (* x x x))

(define (sqrt-cube num)
  (sqrt-cube-iter 1.0 num)
)

(cube 3)
;               without-improve       with-improve
(sqrt-cube 8) ; 2.000004911675504     2.000000000012062
(sqrt-cube 27) ; 3.0000005410641766   3.0000000000000977
(sqrt-cube 270) ; 6.463304085080172   6.46330407009565
(sqrt-cube 100) ; 4.641590111046459   4.64158883361313
