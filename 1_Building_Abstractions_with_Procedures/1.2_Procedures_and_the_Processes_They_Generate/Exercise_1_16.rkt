#lang racket

; Right-to-left binary method for exponentiation
; from section 4.6.3 (page 462) of Seminumerical Algorithms.
; Volume 2 of The Art of Computer Programming. 2nd edition.
;
; A1. [Initialize.] Set N ← n, Y ← 1, Z ← x.
; A2. [Halve N.] (At this point, x^n = Y * Z^N.) Set t ← N mod 2 and N ← ⌊N/2⌋.
;   If t = 0, skip to step A5.
; A3. [Multiply Y by Z.] Set Y ← Z times Y.
; A4. [N = 0?] If N = 0, the algorithm terminates, with Y as the answer.
; A5. [Square Z.] Set Z ← Z times Z, and return to step A2.
;
; Racket manual: https://docs.racket-lang.org/reference/generic-numbers.html

(define (exp x n) (rtl n 1 x))
(define (rtl N Y Z)
    (if (even? N)
        (rtl (quotient N 2) Y (* Z Z))
            (if (zero? (quotient N 2))
                (* Z Y)
                (rtl (quotient N 2) (* Z Y) (* Z Z)))))

(exp 3 2)
; 9
