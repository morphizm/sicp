#lang racket/base

(require rackunit)
(require "mutable-pairs.rkt")

(define (make-deque)
  (cons '() '()))

(define (front-deque deque)
  (car deque))

(define (empty-deque? deque)
  (null? (front-deque deque)))

(define (rear-deque deque)
  (cdr deque))

(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (front-insert-deque! deque item)
  (let ([new-pair (cons item '())])
    (cond
      ((empty-deque? deque)
        (set-front-ptr! deque new-pair)
        (set-rear-ptr! deque new-pair)
        deque)
      (else
        (set-cdr! (rear-deque deque) new-pair)
        deque))))

(define (rear-insert-deque! deque item)
  (let ([new-pair (cons item '())])
    (cond
      ((empty-deque? deque)
        (set-front-ptr! deque new-pair)
        (set-rear-ptr! deque new-pair)
        deque)
      (else
        (set-cdr! (rear-deque deque) new-pair)
        (set-rear-ptr! deque new-pair)
        deque))))

(define (front-delete-deque! deque)
    (cond ((empty-deque? deque)
         (error "DELETE! call with empty deque" deque))
         (else
          (set-front-ptr! deque (cdr (front-deque deque)))
          deque)))

(define (rear-delete-deque! deque)
    (cond ((empty-deque? deque)
         (error "DELETE! call with empty deque" deque))
         (else
          (set-rear-ptr! deque (cdr (rear-deque deque)))
          deque)))

(define d1 (make-deque))
(displayln d1)
(front-insert-deque! d1 'a)
(displayln d1)
(front-insert-deque! d1 'b)
(displayln d1)
(rear-insert-deque! d1 'c)
(displayln d1)


; (insert-queue! q1 'a)
; (insert-queue! q1 'b)
; (delete-queue! q1)
; (delete-queue! q1)
; (print-queue (insert-queue! q1 'a))
; (print-queue (insert-queue! q1 'b))
; (print-queue (delete-queue! q1))
; (print-queue (delete-queue! q1))
