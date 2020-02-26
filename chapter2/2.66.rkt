#lang racket/base

(require "func.rkt")
(require rackunit)

(define (lookup given-key set-of-records)
  (cond
    ((null? set-of-records?) #f)
    ((= given-key (key (entry set-of-records))) (entry set-of-records))
    ((< given-key (key (entry set-of-records))) (lookup (left-branch set-of-records)))
    ((> given-key (key (entry set-of-records))) (lookup (right-branch set-of-records)))
  )
)

