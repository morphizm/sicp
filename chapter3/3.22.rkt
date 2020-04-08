#lang racket/base

(require rackunit)
(require "mutable-pairs.rkt")

(define (make-queue)
  (let ([front-ptr '()]
        [rear-ptr '()])
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (print-queue)
      (displayln front-ptr))
    (define (insert-queue! item)
      (let ([new-pair (cons item '())])
        (cond ((empty-queue?)
              (set-front-ptr! new-pair)
              (set-rear-ptr! new-pair))
              (else
                (set-cdr! rear-ptr new-pair)
                (set-rear-ptr! new-pair)))))
    (define (delete-queue!)
      (cond ((empty-queue?) (error "DELETE! call with empty queue"))
            (else
              (set-front-ptr! (cdr front-ptr)))))
    (define (dispatch m)
      (cond
        ((eq? m 'front-ptr) front-ptr)
        ((eq? m 'rear-ptr) rear-ptr)
        ((eq? m 'set-front-ptr!) set-front-ptr!)
        ((eq? m 'set-rear-ptr!) set-rear-ptr!)
        ((eq? m 'empty-queue?) (empty-queue?))
        ((eq? m 'print-queue) (print-queue))
        ((eq? m 'insert-queue!) insert-queue!)
        ((eq? m 'delete-queue!) (delete-queue!))
        (else (error "Unknown operation DISPATCH -- " m))
      )
    )
    dispatch))

(define q1 (make-queue))
((q1 'insert-queue!) 'a)
(q1 'print-queue)
((q1 'insert-queue!) 'b)
(q1 'print-queue)
(q1 'front-ptr)
(q1 'rear-ptr)

; ((q1 'insert-queue!) 'c)
; (q1 'print-queue)


(q1 'delete-queue!)
(q1 'print-queue)
(q1 'delete-queue!)
(q1 'print-queue)
; (q1 'delete-queue!)
