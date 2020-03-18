#lang racket/base

(require "func.rkt")
(require rackunit)

; number?
; symbol?

; (define (attach-tag type-tag contents)
;   (cons type-tag contents))

; (define (type-tag datum)
;   (if (pair? datum)
;       (car datum)
;       (error "Bad tagged data -- TYPE-TAG" datum)))

; (define (contents datum)
;   (if (pair? datum)
;       (cdr datum)
;       (error "Bad tagged data -- CONTENTS" datum)))
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



(define x (attach-tag 'tag (list 1 2)))
(type-tag x)
(contents x)

(define num (attach-tag 'untag 2))
(type-tag num)
(contents num)