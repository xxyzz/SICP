#lang racket

; Russian peasant method modified from right-to-left binary method:
; change step A3 to “Y ← Y + Z” and step A5 to “Z ← Z + Z”, and set
; Y to zero in step A1, the algorithm terminates with Y = nx.
; See section 4.6.3 (page 462) of Seminumerical Algorithms.
; Volume 2 of The Art of Computer Programming. 2nd edition.

(define (mlp x n) (rtl n 0 x))
(define (rtl N Y Z)
    (if (even? N)
        (rtl (quotient N 2) Y (+ Z Z))
            (if (zero? (quotient N 2))
                (+ Z Y)
                (rtl (quotient N 2) (+ Z Y) (+ Z Z)))))

(define (double x) (+ x x))

; divides an (even) integer by 2
(define (halve x) (/ x 2))

(define (mlp x n)
    (define (helper X N Y)
        (cond [(zero? N) Y]
              [(even? N) (helper (double X) (halve N) Y)]
              [else (helper X (sub1 N) (+ X Y))]))
            
(helper x n 0))

(mlp 3 2)
; 6
