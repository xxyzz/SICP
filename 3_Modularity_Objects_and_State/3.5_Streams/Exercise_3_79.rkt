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

(define (solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

(stream-ref (solve-2nd (lambda (dy y) dy) 0.001 1 1) 1000)
; 2.716923932235896
