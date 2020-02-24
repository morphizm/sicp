#lang racket/base

(require rackunit)

(define (even? n)
  (= (remainder n 2) 0)
)

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))
  )  
)

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))

  (* (sum f (+ a (/ dx 2)) add-dx b) dx)
)

(define (simpson-integral f a b n) 
  (define h (/ (- b a) n)) 
  (define (g k) 
    (define (c k) 
      (cond ((= k 0) 1) 
            ((= k n) 1) 
            ((even? k) 2) 
            (else 4))) 
    (* (c k) (f (+ a (* k h))))) 
  (define (inc k) (+ k 1)) 
  (/ (* (sum g 0 inc n) h) 3))

(define (cube x) (* x x x))
(integral cube 0 1 0.01)
(simpson-integral cube 0.0 1 100)