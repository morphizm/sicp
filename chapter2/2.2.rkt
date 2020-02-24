#lang racket/base

(require rackunit)

(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point)
)
(define (y-point point)
  (cdr point)
)

(define (make-segment point1 point2)
  (cons point1 point2)
)
(define (start-segment segment)
  (car segment)
)
(define (end-segment segment)
  (cdr segment)
)

(define (average a b) 
  (/ (+ a b) 2.0))

(define (midpoint-segment segment) 
  (make-point (average (x-point (start-segment segment)) 
                       (x-point (end-segment segment))) 
              (average (y-point (start-segment segment)) 
                       (y-point (end-segment segment)))))

(midpoint-segment (make-segment (make-point 1 0) (make-point 2 0)))
