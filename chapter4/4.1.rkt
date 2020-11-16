#lang racket/base
(require "./func.rkt")
(require "./lisp.rkt")


(define (last-operand exps) exps)
(define (operands-1 exps) exps)

(define (list-of-values-left exps env)
  (if (no-operands? exps)
      nil
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (list-of-values-right exps env)
  (if (no-operands? exps)
      nil
      (cons (eval (last-operand exps) env)
            (list-of-values (operands-1 exps) env))))
