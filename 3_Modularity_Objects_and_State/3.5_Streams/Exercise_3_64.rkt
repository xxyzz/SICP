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

(define (square x ) (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))   ; Sn-1
        (s1 (stream-ref s 1))   ; Sn
        (s2 (stream-ref s 2)))  ; Sn+1
    (stream-cons (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-rest s)))))

(define (make-tableau transform s)
  (stream-cons s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-first (make-tableau transform s)))

(define (stream-limit stream tolerance)
  (let ([first (stream-ref stream 0)]
        [second (stream-ref stream 1)])
    (if (< (abs (- first second)) tolerance)
        second
        (stream-limit (stream-rest stream) tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2 0.000001)
; 1.414213562373095

(stream->list (stream-take (sqrt-stream 2) 8))
; '(1.0 1.5 1.4166666666666665 1.4142156862745097 1.4142135623746899 1.414213562373095 1.414213562373095 1.414213562373095)

(stream->list (stream-take (accelerated-sequence euler-transform (sqrt-stream 2)) 8))
; '(1.0 1.4285714285714284 1.414213201613577 1.414213562373095 +nan.0 +nan.0 +nan.0 +nan.0)
