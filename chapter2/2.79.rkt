#lang racket/base

(require "func.rkt")
(require "complex.rkt")
(require rackunit)

(define (attach-tag type-tag contents)
  (cond
    ((number? contents) contents)
    (else (cons type-tag contents))))

(define (type-tag datum)
  (cond 
      ((number? datum) 'scheme-number)
      ((pair? datum) (car datum))
      (else (error "Bad tagged data -- TYPE-TAG" datum))))

(define (contents datum)
  (cond 
      ((number? datum) datum)
      ((pair? datum) (cdr datum))
      (else (error "Bad tagged data -- CONTENTS" datum))))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (equal-complex? x y)
  (let ([real-eq? (eq? (real-part x) (real-part y))]
        [imag-eq? (eq? (imag-part x) (imag-part y))]
  ) (and real-eq? imag-eq?)))

(define (equ? num1 num2)
  (cond
    ((eq? (type-tag num1) 'scheme-number) (= (contents num1) (contents num2)))
    ((eq? (type-tag num1) 'rational) (equal-rat? (contents num1) (contents num2)))
    ((eq? (type-tag num1) 'complex) (equal-complex? (contents num1) (contents num2)))
    
    (else (error "Bad number" num1 num2))
  )
)

(equ? 1 2)
(equ? 1 1)


(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define rectangular (make-from-real-imag-rectangular 1 2))
(define complex (attach-tag 'complex rectangular))
(define rectangular2 (make-from-real-imag-rectangular 1 3))
(define complex2 (attach-tag 'complex rectangular2))

(equ? complex complex)
(equ? complex complex2)


;(define (equ? x y) (apply-generic 'equ? x y))
;(put 'equ? '(scheme-number scheme-number) =)