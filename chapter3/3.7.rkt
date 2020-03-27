#lang racket/base

(require rackunit)

(define (make-joint account account-password password)
   (define (dispatch pas m)
    (if (not (eq? pas password))
        (lambda (a . rest) "Wrong password")
        (cond
          ((eq? m 'withdraw) (account account-password 'withdraw))
          ((eq? m 'deposit) (account account-password 'deposit))
          (else (error "Unknown call -- MAKE-ACCOUNT" m)))))
  dispatch)

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

(define my-acc (make-account 100 'secret-password))
(define joint-acc (make-joint my-acc 'secret-password 'joint-password))

((my-acc 'secret 'withdraw) 20)
((joint-acc 'secret 'withdraw) 20)


(check-equal? ((my-acc 'secret-password 'withdraw) 6) 94)
(check-equal? ((my-acc 'secret-password 'withdraw) 6) 88)
(check-equal? ((joint-acc 'joint-password 'withdraw) 6) 82)
(check-equal? ((my-acc 'secret-password 'withdraw) 6) 76)
(check-equal? ((joint-acc 'joint-password 'withdraw) 6) 70)
(check-equal? ((joint-acc 'joint-password 'withdraw) 6) 64)
(check-equal? ((my-acc 'secret-password 'withdraw) 6) 58)
(check-equal? ((my-acc 'secret-password 'deposit) 10) 68)
(check-equal? ((joint-acc 'joint-password 'deposit) 10) 78)



