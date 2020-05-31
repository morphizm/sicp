#lang racket/base

(require rackunit)
(require "mutable-pairs.rkt")

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error ("FRONT call with empty queue" queue))
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ([new-pair (cons item '())])
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
           (else
            (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! call with empty queue" queue))
         (else
          (set-front-ptr! queue (cdr (front-ptr queue)))
          queue)))


(define (make-deque)
  (cons (make-queue) (make-queue)))

(define (front-deque deque)
  (car deque))

(define (empty-deque? deque)
  (null? (front-deque deque)))

(define (rear-deque deque)
  (cdr deque))

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
; (front-insert-deque! d1 'b)
; (displayln d1)
; (rear-insert-deque! d1 'c)
; (displayln d1)


; (insert-queue! q1 'a)
; (insert-queue! q1 'b)
; (delete-queue! q1)
; (delete-queue! q1)
; (print-queue (insert-queue! q1 'a))
; (print-queue (insert-queue! q1 'b))
; (print-queue (delete-queue! q1))
; (print-queue (delete-queue! q1))
