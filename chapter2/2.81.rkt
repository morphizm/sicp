#lang racket/base

(require "func.rkt")
(require "complex.rkt")
(require rackunit)

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

(define (exp x y) (apply-generic 'exp x y))
(put 'exp '(scheme-number scheme-number)
  (lambda (x y) (tag (expt x y))))

; exec(exp with two complex)?
; apply-generic не найдет операции, попытается привести complex -> complex.
; Ничего не найдет, выкинет ошибку

; Если добавить операции определенные выше, то apply-generic зациклится

(define (apply-generic op . args)
  (let ([type-tags (map type-tag args)])
    (let ([proc (get op type-tags)])
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2) (eq? (car type-tags) (cadr type-tags)))
              (let ([type1 (car type-tags)]
                    [type2 (cadr type-tags)]
                    [a1 (car args)]
                    [a2 (cadr args)])
                (let ([t1->t2 (get-coercion type1 type2)]
                      [t2->t1 (get-coercion type2 type1)])
                  (cond
                        (t1->t2 (apply-generic op (t1->t2 a1) a2))
                        (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for this types" (list op type-tags))))))
              (error "No method for this types" (list op type-tags)))))))
