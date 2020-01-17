#lang racket/base

(define tree (generate-huffman-tree '((A 16) (B 8) (C 4) (D 2) (E 1))))
; '(((((leaf E 1) (leaf D 2) (E D) 3) (leaf C 4) (E D C) 7) (leaf B 8) (E D C B) 15) (leaf A 16) (E D C B A) 31)
;                E D C B A 11
;                     /     \
;            E D C B 15      A 16
;               /     \
;           E D C 7   B 8
;           /   \
;      E D 3     C 4
;       /  \
;    E 1   D 2

(encode '(A) tree)
; '(1)

(encode '(E) tree)
; '(0 0 0 0)

(define tree (generate-huffman-tree '((A 512) (B 256) (C 128) (D 64) (E 32) (F 16) (G 8) (H 4) (I 2) (J 1))))
; '((((((((((leaf J 1) (leaf I 2) (J I) 3) (leaf H 4) (J I H) 7) (leaf G 8) (J I H G) 15) (leaf F 16) (J I H G F) 31) (leaf E 32) (J I H G F E) 63) (leaf D 64) (J I H G F E D) 127) (leaf C 128) (J I H G F E D C) 255) (leaf B 256) (J I H G F E D C B) 511) (leaf A 512) (J I H G F E D C B A) 1023)

(encode '(A) tree)
; '(1)

(encode '(J) tree)
; '(0 0 0 0 0 0 0 0 0)

; most frequent symbol: 1 bit
; least frequent: n - 1 bits
