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

(define dx 0.00001)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
      (f x)
      (f (+ x dx))
    ) 3)
  )
)

(define (n-fold-smoothed f n)
  ((repeated smooth n) f)
)

; (check-equal?)
((smooth square) 2)

((n-fold-smoothed square 17) 2)

; (define (smooth f dx)
;   (define (average x y z) (/ (+ x y z) 3))
;   (lambda (x) (average (f (- x dx))
;                        (f x)
;                        (f (+ x dx)))))

;                        (define (smooth-n f dx n)
;   ((repeated (lambda (g) (smooth g dx)) n) f))
;   ((smooth-n square 0.00001 10) 4)


; (define (smooth-n f dx n)
;   (repeated (smooth f dx) n))