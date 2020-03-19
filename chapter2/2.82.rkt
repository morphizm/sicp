#lang racket/base

(require "func.rkt")
(require "complex.rkt")
(require rackunit)

(define (apply-generic-two-args op . args)
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

(define (apply-generic op . args)
  (if (= (length args) 2)
      (apply-generic-two-args op args)
      (apply
        apply-generic
        op
        (append (list (apply-generic-two-args op (list (car args) (cadr args))))
              (cdr (cdr args))
        )
      )
  )
)

; (list num1 num2 num3)

(define (cycle op . args)
  (if (= (length args) 2)
      (+ (car args) (cadr args))
      (apply cycle
           op (append (list (+ (car args) (cadr args)))
          (cdr (cdr args))))))

(cycle 'op 1 2 3 4)
