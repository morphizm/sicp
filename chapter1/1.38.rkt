#lang racket/base

(require rackunit)

; (define (cont-frac n d k i d+)
;   (cond ((= i k) (/ (n i) (if (d i) d+ 1)))
;     (else (/ (n i) (+ (if (d i) d+ 1) (cont-frac n d k (+ i 1) (if (d i) (+ 2 d+) d+)))))
;   )
; )

(define (cont-frac n d k i d+)
  (let ((current-d (if (d i) d+ 1)))
    (cond ((= i k) (/ (n i) current-d))
      (else (/ (n i) (+ current-d (cont-frac n d k (+ i 1) (if (= current-d 1) d+ (+ 2 d+))))))
    )
  )
)

(cont-frac
  (lambda (i) 1.0)
  (lambda (i) (= (remainder (- i 1) 3) 0)
  )
  10
  0
  2
)

(- 2.718281828459045 2)
