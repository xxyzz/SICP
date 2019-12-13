#lang racket/base

(define (iterative-improve good-enough? improve)
    (lambda (first-guess)
        (define (iter guess)
            (if (good-enough? guess)
                guess
                (iter (improve guess))))
        (iter first-guess)))

(define (average x y) (/ (+ x y) 2))

(define (square x) (* x x))

(define tolerance 0.00001)

(define (sqrt x)
    ((iterative-improve
        (lambda (guess)
            (< (abs (- (square guess) x)) tolerance))
        (lambda (guess)
            (average guess (/ x guess))))
    1.0))

(sqrt 9)
; 3.000000001396984

(define (fixed-point f)
    ((iterative-improve
        (lambda (guess)
            (< (abs (- (f guess) guess)) tolerance))
        f)
    1.0))

(fixed-point cos)
; 0.7390893414033928

(fixed-point (lambda (y) (+ (sin y) (cos y))))
; 1.2587228743052672
