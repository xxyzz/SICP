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

(define (RLC R L C dt)
  (define (RLC-circuit vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay diL) iL0 dt))
    (define dvC (scale-stream iL (/ -1 C)))
    (define diL (add-streams (scale-stream vC (/ 1 L))
                             (scale-stream iL (* -1 R (/ 1 L)))))
    (cons vC iL))
  RLC-circuit)

(define RLC1 (RLC 1 1 0.2 0.1))
(define stream-pair (RLC1 10 0))
(stream-ref (car stream-pair) 10)
; -3.5160605800000004
(stream-ref (cdr stream-pair) 10)
; 2.750945989
