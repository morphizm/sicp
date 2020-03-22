#lang racket/base

(require "func.rkt")
(require "complex.rkt")
(require rackunit)

(define (is-high? type1 type2)
  (cond 
    ((eq? type1 type2) #f)
    ((eq? type1 'scheme-number) #f)
    ((and (eq? type1 'rational) (or (eq? type2 'real) (eq? type2 'complex))) #f)
    ((and (eq? type1 'real) (eq? type2 'complex)) #f)
    (else #t)
  )
)

(define (apply-generic op . args)
  (let ([type-tags (map type-tag args)])
    (let ([proc (get op type-tags)])
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2))
              (let ([type1 (car type-tags)]
                    [type2 (cadr type-tags)]
                    [a1 (car args)]
                    [a2 (cadr args)])
                (let ([is-same-types (eq? type1 type2)]
                      [raised-type (if (is-high? type2 type1) type1 type2)]
                      [same-type (if (is-high? type2 type1) type2 type1)])
                  (cond
                        (is-same-types (error "No method for this types" (list op type-tags)))
                        (else 
                            (if (is-high? type2 type1)
                                (apply-generic op (raise type1 a1) (list type2 a2))
                                (apply-generic op (raise type2 a2) (list type1 a1))
                            )
                        ))))
              (error "No method for this types" (list op type-tags)))))))

