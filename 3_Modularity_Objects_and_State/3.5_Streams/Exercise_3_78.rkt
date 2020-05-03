#lang racket/base
(require racket/stream)
(require racket/promise)

(define (integral delayed-integrand initial-value dt)
  (stream-cons
   initial-value
   (let ([integrand (force delayed-integrand)])
     (if (stream-empty? integrand)
         empty-stream
         (integral (delay (stream-rest integrand))
                   (+ (* dt (stream-first integrand))
                      initial-value)
                   dt)))))

(define (stream-map proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-map
              (cons proc (map stream-rest argstreams))))))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define (integral-b delayed-integrand initial-value dt)
  (define int
    (stream-cons
     initial-value
     (let ([integrand (force delayed-integrand)])
       (add-streams (scale-stream integrand dt) int))))
  int)

(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a) (scale-stream y b)))
  y)

(stream-ref (solve-2nd 1 0 0.001 1 1) 1000)
; 2.716923932235896

; https://www.mathsisfun.com/calculus/differential-equations-second-order.html
