#lang racket/base

;; Right-to-left binary method for exponentiation
;; from section 4.6.3 (page 462) of Seminumerical Algorithms.
;; Volume 2 of The Art of Computer Programming. 2nd edition.

;; This algorithm evaluates x^n, where n is a positive integer.
;; (Here x belongs to any algebraic system in which an associative
;; multiplication, with identity element 1, has been defined.)
;; A1. [Initialize.] Set N ← n, Y ← 1, Z ← x.
;; A2. [Halve N.] (At this point, x^n = Y * Z^N.) Set t ← N mod 2 and N ← ⌊N/2⌋.
;;   If t = 0, skip to step A5.
;; A3. [Multiply Y by Z.] Set Y ← Z times Y.
;; A4. [N = 0?] If N = 0, the algorithm terminates, with Y as the answer.
;; A5. [Square Z.] Set Z ← Z times Z, and return to step A2.

;; Racket manual: https://docs.racket-lang.org/reference/generic-numbers.html

(define (exp x n) (rtl n 1 x))
(define (rtl N Y Z)
  (let ([t (remainder N 2)]
        [n (quotient N 2)]
        [y (* Z Y)]
        [square-z (* Z Z)])
    (cond [(zero? N) 1]  ;; 0 is not a positive integer
          [(zero? t) (rtl n Y square-z)]
          [(zero? n) y]
          [else (rtl n y square-z)])))

(exp 3 2)
;; 9
(exp 3 0)
;; 1

;; why it's called right-to-left binary method:
;; 23=(10111)₂=16+4+2+1
;; x²³ = x¹⁶ * x⁴ * x² * x
;; x -> x² -> x⁴ -> x⁸ -> x¹⁶
;; it takes 3 + 4 + 1(x * 1, step A3) multiplications
;; the algorithm takes ⌊lg n⌋ + ν(n) multiplications
;; ν(n) is the number of ones in the binary representation of n
