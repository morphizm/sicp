#lang racket/base
(require "./func.rkt")
(require racket/promise)

(define stream-null? null?)
(define the-empty-stream '())
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

; cons-stream - special form
; cons-stream <a> <b> == cons <a> (delay <b>)

(define (cons-stream a b) (cons a (delay b)))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond [(stream-null? stream) the-empty-stream]
        [(pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream)))]
        [else (stream-filter pred (stream-cdr stream))]))

(define p2
  (stream-car
    (stream-cdr
      (stream-filter prime?
                     (stream-enumerate-interval 10000 1000000)))))

(define p2-sync
  (car
    (cdr
      (filter prime?
             (enumerate-interval 10000 1000000)))))
(display p2)
; ------------------------------------------------
; delay <expression>, expression == lambda () <expression>
(define (memo-proc proc)
  (let [(already-run? #f) (result #f)]
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))
(define (force-ex delayed-object)
  (delayed-object))


(display-stream
  (cons-stream 1 (cons-stream 2 (cons-stream 3 the-empty-stream))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

(stream-ref no-sevens 100)
(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

(define (sieve stream)
  (cons-stream
    (stream-car stream)
    (sieve (stream-filter
             (lambda (x)
               (not (divisible? x (stream-car stream))))
             (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(stream-ref primes 50)

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))
(define fibs
  (cons-streams 0
                (cons-stream 1
                             (add-streams (stream-cdr fibs)
                                          fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2))))
(define primes
  (cons-stream
    2
    (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond [(> (square (stream-car ps)) n) #t]
          [(divisible? n (stream-car ps)) #f]
          [else (iter (stream-cdr ps))]))
  (iter primes))


(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                                (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(display-stream (sqrt-stream 2))
(display-stream pi-stream)

(define (euler-transform s)
  (let [(s0 (stream-ref s 0))] ; S n-1
       [(s1 (stream-ref s 1))] ; S n
       [(s2 (stream-ref s 2))] ; S n+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(display-stream (euler-transform pi-stream))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))
(display-stream (accelerated-sequence euler-transform pi-stream))

(stream-filter (lambda (pair)
                (prime? (+ (car pair) (cadr pair))))
               int-pairs)

(stream-map (lambda (x) (list (stream-car s) x))
            (stream-cdr t))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x))
                 (stream-cdr t))
     (pairs (stream-cdr s) (stream-cdr t)))))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))


