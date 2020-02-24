#lang racket/base

(provide accumulate append enumerate-interval nil
  reverse filter enumerate-tree prime? accumulate-n
  length flatmap square sum permutations)

(require rackunit)
(define nil '())

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(define (reverse items)
  (define (iter elems acc)
    (if (null? elems)
      acc
      (iter (cdr elems) (cons (car elems) acc))))
  (iter items list))

(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items)) (map proc (cdr items)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
    ((predicate (car sequence)) 
      (cons (car sequence) (filter predicate (cdr sequence))))
    (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
      (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op init (map (lambda (x) (car x)) seqs))
          (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
    ((not (pair? tree)) (list tree))
    (else (append (enumerate-tree (car tree))
                  (enumerate-tree (cdr tree))))))

(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (length sequence)
  (accumulate (lambda (x acc) (+ 1 acc)) 0 sequence))

(define (square x) (* x x))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (next test-divisor)))))

(define (next num)
  (if (= num 2) 3 (+ num 2)))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
  (if (= n 1) #f (= n (smallest-divisor n))))

(define (sum elems)
  (accumulate + 0 elems))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence)
)

(define (permutations s)
  (if (null? s)
    (list nil)
    (flatmap (lambda (x)
                (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
              s)))