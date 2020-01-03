#lang racket/base

(define (square x) (* x x))

(define (expmod base exp m)
    (cond [(= exp 0) 1]
          [(even? exp)
              (remainder
                  (square (expmod base (/ exp 2) m))
                  m)]
          [else
              (remainder
                  (* base (expmod base (- exp 1) m))
                  m)]))

(define (fermat-test n a)
    (cond [(= a 1) #t]
          [(= (expmod a n n) a) (fermat-test n (sub1 a))]
          [else #f]))

(define (fool n) (fermat-test n (sub1 n)))

(fool 3)
; #t not fooled

(fool 4)
; #f not fooled

(fool 561)
; #t fooled! 561 / 3 = 187

(fool 1105)
; #t fooled! 1105 / 5 = 221

(fool 1729)
; #t fooled! 1729 / 7 = 247

(fool 2465)
; #t fooled! 1729 / 5 = 493

(fool 2821)
; #t fooled! 2821 / 7 = 403

(fool 6601)
; #t fooled! 6601 / 7 = 943
