#lang racket/base

(define (sqrt-iter guess x)
  (displayln guess)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (old-good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (good-enough? guess x)
  (if (= guess 0)
      0
      (< (/ (abs (- (improve guess x) guess)) guess) 0.00001)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; old good-enough?
(sqrt 0.0001)
;; 0.03230844833048122
(- (expt 0.03230844833048122 2) 0.0001)
;; 0.0009438358335233747 < 0.001
(sqrt 8796093022208)
;; 1.0
;; 4398046511104.5
;; 2199023255553.25
;; 1099511627778.625
;; 549755813893.3125
;; 274877906954.65625
;; 137438953493.32812
;; 68719476778.66406
;; 34359738453.33203
;; 17179869354.666016
;; 8589934933.333006
;; 4294967978.666483
;; 2147485013.333079
;; 1073744554.6652374
;; 536876373.3222021
;; 268446378.57776874
;; 134239572.62225026
;; 67152548.97883326
;; 33641767.856056966
;; 16951615.65799945
;; 8735254.869319363
;; 4871109.768821579
;; 3338438.769158059
;; 2986615.573400899
;; 2965893.1941674994
;; 2965820.801641373
;; 2965820.8007578608
;; 2965820.8007578608
(abs (- (expt 2965820.8007578608 2) 8796093022208))
;; 0.001953125 > 0.001
(/ (+ 2965820.8007578608
      (/ 8796093022208 2965820.8007578608))
   2)
;; 2965820.8007578608
;; can't improve anymore but still not good enough
;; infinite loop

;; Why 8796093022208?
;; The smallest significand of double-precision floating-point number
;; is 2^-52, if it has 8 bits exponent and 53 bits precision.
(log (/ 0.001 (expt 2 -52)) 2)
;; 42.034215715337915
;; when the exponent is 2^43, gaps between the floating-point numbers
;; are:
(* (expt 2 43) (expt 2 -52))
;; 2^-9 = 0.001953125 > 0.001
;; When a double number greater or equal then 2^43 = 8796093022208,
;; the gaps between them are greater or equal then 2^-9. When it's lower
;; then 2^43, the gaps are lower or equal then 2^-10 = 0.0009765625
;; 2^42*(1 + 2^-1 + ... + 2^-52), 2^43 * 1, 2^43 * (1 + 2^-52)
(exact->inexact (- (expt 2 43) (* (expt 2 43) (- 1 (expt 2 -53)))))
;; 0.0009765625 = 2^-10
(exact->inexact (* (expt 2 43) (- 1 (expt 2 -53))))
;; 8796093022207.999
(sqrt 8796093022207.999)
;; 2965820.8007578608 *bleep* yeah!

;; new good-enough?
(sqrt 0.0001)
;; 0.010000000025490743
(sqrt 8796093022208)
;; 2965820.801641373

;; RTFM
;; https://github.com/xxyzz/c/blob/master/2/2-1.c
