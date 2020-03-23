#lang racket/base

(require rackunit)

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Not enough money on account"))
  (define (deposit amount) (begin (set! balance (+ balance amount)) balance))
  (define (dispatch pas m)
    (if (not (eq? pas password))
        (lambda (a . rest) "Wrong password")
        (cond
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown call -- MAKE-ACCOUNT" m)))))
  dispatch)

(define acc (make-account 100 'secret-password))

(check-equal? ((acc 'secret-password 'withdraw) 60) 40)
((acc 'secret 'withdraw) 20)
