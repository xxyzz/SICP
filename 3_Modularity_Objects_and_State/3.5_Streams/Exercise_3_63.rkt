#lang racket/base
(require racket/stream)

(define (average x y)
    (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (stream-cons
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)

; Louis Reasoner's version
(define (sqrt-stream x) 
  (stream-cons 1.0 (stream-map
                    (lambda (guess)
                      (sqrt-improve guess x))
                    (sqrt-stream x))))
; it will create new streams each recursion thus can't memory results.
