#lang racket/base

(require "func.rkt")
(require rackunit)

(define (deriv exp var)
  (cond 
    ((number? exp) 0)
    ((variable? exp)
      (if (same-variable? exp var) 1 0))
    ((sum? exp)
      (make-sum (deriv (addend exp) var) 
        (deriv (augend exp) var)))
    ((product? exp)
      (make-sum 
        (make-product (multiplier exp) 
                      (deriv (multiplicand exp) var))  
        (make-product (deriv (multiplier exp) var)
                      (multiplicand exp))))
    ((exponentiation? exp)
      (make-product
        (exponent exp)
        (make-exponentiation (base exp) (exponent exp))))
    (else 
      (error "undedined expression -- DERIV" exp))
  )
)

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) 
  (cond
    ((=number? a1 0) a2)
    ((=number? a2 0) a1)
    ((and (number? a1) (number? a2)) (+ a1 a2))
    (else (list '+ a1 a2))))  

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2) 
  (cond
    ((or (=number? m1 0) (=number? m2 0)) 0)
    ((=number? m1 1) m2)
    ((=number? m2 1) m1)
    ((and (number? m1) (number? m2)) (* m1 m2))
    (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))
(define (augend s)
  (accumulate
    (lambda (x acc) (make-sum x acc))
    0
    (cdr (cdr s))
  ))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))
(define (make-exponentiation b e)
  (let ([new-exp (subtract e 1)])
    (cond
      ((=number? new-exp 0) 1)
      ((=number? new-exp 1) b)
      (else (list '** b new-exp))
    )))

(define (subtract sub num)
  (cond
    ((number? sub) (- sub num))
    (else (list '- sub num))))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

(deriv '(** x 2) 'x)
(deriv '(** x (* x 2)) 'x)

(deriv '(* x (+ x 3 2 2)) 'x)
(deriv '(+ x (* x 2) y u) 'x)
