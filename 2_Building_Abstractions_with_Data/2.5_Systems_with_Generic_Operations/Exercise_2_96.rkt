#lang racket/base

; a:
(define (gcd-terms a b)
  (display "gcd-terms a ")
  (display a)
  (display "\n")
  (display "gcd-terms b ")
  (display b)
  (display "\n\n")
  (if (empty-termlist? b)
    a
    (gcd-terms b (pseudoremainder-terms a b))))

(define (pseudoremainder-terms a b)
  (let* ([O1 (order (first-term a))]
         [O2 (order (first-term b))]
         [c (coeff (first-term b))]
         [int-factor (expt c (add1 (- O1 O2)))]
         [new-a (mul-term-by-all-terms (make-term 0 int-factor) a)])
    (cadr (div-terms new-a b))))

(define p1 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
(define p2 (make-polynomial 'x '((2 11) (0 7))))
(define p3 (make-polynomial 'x '((1 13) (0 5))))
(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

(greatest-common-divisor q1 q2)

; gcd-terms a ((4 11) (3 -22) (2 18) (1 -14) (0 7))
; gcd-terms b ((3 13) (2 -21) (1 3) (0 5))

; div-term L1 ((4 1859) (3 -3718) (2 3042) (1 -2366) (0 1183))
; div-term L2 ((3 13) (2 -21) (1 3) (0 5))

; div-term L1 ((3 -715) (2 2613) (1 -3081) (0 1183))
; div-term L2 ((3 13) (2 -21) (1 3) (0 5))

; div-term L1 ((2 1458) (1 -2916) (0 1458))
; div-term L2 ((3 13) (2 -21) (1 3) (0 5))

; gcd-terms a ((3 13) (2 -21) (1 3) (0 5))
; gcd-terms b ((2 1458) (1 -2916) (0 1458))

; div-term L1 ((3 27634932) (2 -44641044) (1 6377292) (0 10628820))
; div-term L2 ((2 1458) (1 -2916) (0 1458))

; div-term L1 ((2 10628820) (1 -21257640) (0 10628820))
; div-term L2 ((2 1458) (1 -2916) (0 1458))

; div-term L1 ()
; div-term L2 ((2 1458) (1 -2916) (0 1458))

; gcd-terms a ((2 1458) (1 -2916) (0 1458))
; gcd-terms b ()

; '(polynomial x (2 1458) (1 -2916) (0 1458))

; b:
