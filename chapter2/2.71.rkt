#lang racket/base

(require "func.rkt")
(require "haffman.rkt")
(require "2.68.rkt")
(require "2.69.rkt")
(require rackunit)

; n = 5 => 2 ** 5 = 32
(define pairs-5
  '(
    (A 16)
    (B 8)
    (C 4)
    (D 2)
    (F 1)
  )
)

(define tree-5 (generate-huffman-tree pairs-5))
(encode '(A) tree-5); 0
(encode '(F) tree-5); 1111
; 4 раза

; n = 10 => 2 ** 10 = 1024
(define pairs-10
  '(
    (A 512)
    (B 256)
    (C 128)
    (D 64)
    (E 32)
    (F 16)
    (G 8)
    (H 4)
    (I 2)
    (J 1)
  )
)

(define tree-10 (generate-huffman-tree pairs-10))
(encode '(A) tree-10); 0
(encode '(J) tree-10); 1111 1111 1
; 9 раз
; 2.25 раз

(define pairs-100
  '(
    (A 512)
    (B 256)
    (C 128)
    (D 64)
    (E 32)
    (F 16)
    (G 8)
    (H 4)
    (I 2)
    (JJ 10)
    (AA 512)
    (BA 256)
    (CA 128)
    (DA 64)
    (EA 32)
    (FA 16)
    (GA 8)
    (HA 4)
    (IA 2)
    (J 1)
  )
)

(define tree-100 (generate-huffman-tree pairs-100))
; (encode '(A) tree-100)
; (length (encode '(J) tree-100))

