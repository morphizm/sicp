#lang racket/base

(require "func.rkt")
(require "haffman.rkt")
(require "2.68.rkt")
(require "2.69.rkt")
(require rackunit)

(define pairs
  '(
    (A 2)
    (BOOM 1)
    (GET 2)
    (JOB 2)
    (NA 16)
    (SHA 3)
    (YIP 9)
    (WAH 1)
  )
)
; n = 8
; log2 8 = 3 => three bit for each symbol
(define tree (generate-huffman-tree pairs))
(define message
  '(
    GET A JOB
    SHA NA NA NA NA NA NA NA NA
    GET A JOB
    SHA NA NA NA NA NA NA NA NA
    WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
    SHA BOOM
  )
)
(define encoded-message (encode message tree))
; 87 bits
; 36 * 3 = 99 bits
; message-length * bit for each
encoded-message