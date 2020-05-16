#lang racket/base
(require racket/function) ; identity

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ([next (f guess)])
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (iter g i)
    (if (< i 1)
        g
        (iter (compose f g) (sub1 i))))
  (iter identity n))

(define (test n x damp-times)
  (fixed-point
   ((repeated average-damp damp-times) (lambda (y) (/ x (expt y (sub1 n)))))
   1.0))

(test 1 2 1)
; 1.9999923706054688

(test 2 2 1)
; 1.4142135623746899

(test 3 27 1)
; 2.9999972321057697
(test 3 27 2)
; 3.000001464168659

(test 4 16 2)
; 2.0000000000021965

(test 5 32 2)
; 2.000001512995761

(test 6 729 1)
; 3.000006257266186
(test 6 729 2)
; 2.999996785898161 faster

(test 7 2187 1)
; 2.9999957809730473
(test 7 2187 2)
; 3.0000041735235943 way more faster

(test 8 6561 1)
; 2.999993884879518
(test 8 6561 2) ; runs like a sloth
(test 8 6561 3)
; 3.0000000000173292

; check out this amazing post:
; https://deltam.blogspot.com/2015/08/sicp145ex145.html

; try plot y = 2/x, y = x, y = 1/2(x + 1/2(x+2/x))
; y=2/x is symmetric to y=x, that will form a loop.
; damp to y = 1/2(x + 1/2(x+2/x)), it's asymmetric now.

; 1/2^k * ((2^k - 1)x + m/x^(n-1)) - m/x^(n-1)
; = (2^k - 1)(x^n - m)/(2^k * x^(n-1))
; when x is at left of the fixed point, damped function is lower then
; the origin function. when x is at the right of the fixed point, demaped
; function is higher. Thus damped function moves the lowest point to the left.

; When the lowest point is on the left of y=x, it's faster
; to find the fixed point.

; damp-times > log2(n)
