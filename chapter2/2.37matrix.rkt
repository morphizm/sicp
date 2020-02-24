#lang racket/base

(require rackunit)
(define nil '())

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op init (map (lambda (x) (car x)) seqs))
          (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
      (accumulate op initial (cdr sequence)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map
    (lambda (x) 
      (accumulate + 0  
        (accumulate-n (lambda (n acc) (* n acc)) 1 (list x v))))
    m)
)

(define (transpose mat)
  (accumulate-n
    (lambda (x acc) (cons x acc))
    nil
    mat))


(define (matrix-*-matrix m n)
  (let ([cols (transpose n)])
    (map 
      (lambda (x) (matrix-*-vector cols x))
      m)
  )
)

(define x (list 1 2 3 4))
(define y (list 9 8 7 6))

(define mx (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(dot-product x y)

(matrix-*-vector mx x)
(define mx*vector-x (list 30 56 80))
; mx x => 
; (list 30 56 80)

(transpose mx)
(define transpose-mx (list (list 1 4 6) (list 2 5 7) (list 3 6 8) (list 4 6 9)))

(matrix-*-matrix mx mx)
(define mtrx (list (list 1 4 6) (list 2 5 7) (list 3 6 8) (list 4 6 9)))
(define mx*mtrx (list (list 30 56 80) (list 56 113 161) (list 80 161 230)))
; mx * mtrx =>
; (list (list 30 56 80) (list 56 113 161) (list 80 161 230)))


(check-equal? (transpose mx) transpose-mx)
(check-equal? (matrix-*-vector mx x) mx*vector-x)
(check-equal? (matrix-*-matrix mx mtrx) mx*mtrx)
