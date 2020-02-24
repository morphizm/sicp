#lang racket/base

(require rackunit)

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (inc x) (+ 1 x))

; (inc 10)

(define (double proc)
  (lambda (x) (proc (proc x)))
)
; ((double inc) 1)

(define (compose f g)
  (lambda (x) (f (g x)))
)

(define (repeated f n)
  (lambda (x)
    (if (= n 1)
      (f x)
      ((repeated f (- n 1)) (f x))
    )
  )
)


(define (sqrt117 x)
  (sqrt-iter 1.0 x)
)
(define (sqrt-iter guess x)
(define (improve guess x)
  (average guess (/ x guess))
)
(define (average x y)
  (/ (+ x y) 2)
)
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001)
)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)
  )
)


(define tolerance 0.00001)

(define (fixed-point133 f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next)
      )
    )
  )
  (try first-guess)
)

(define (iterative-improve good? improve)
  (lambda (x)
    (let ([next (improve x)])
      (if (good? x next)
        next
        ((iterative-improve good? improve) next)
      )
    )
  )
)

(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess))
  )
  (define (average x y)
    (/ (+ x y) 2)
  )
  (define (good-enough? guess next)
    (< (abs (- (square guess) x)) 0.001)
  )

  ((iterative-improve good-enough? improve) x)
)

(sqrt 4.0)


(define (fixed-point f f-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (improve x) (f x))

  ((iterative-improve close-enough? improve) f-guess)
)

(fixed-point cos 1.0)
(fixed-point133 cos 1.0)


