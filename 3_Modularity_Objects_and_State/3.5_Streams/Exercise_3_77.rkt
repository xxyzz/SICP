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

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)
; 0.001 * 1000 = 1
; y = e^t
; 2.716923932235896

; 3b1b differential equations:
; https://www.youtube.com/playlist?list=PLZHQObOWTQDNPOjrT6KVlfJuKtYTftqH6

; mathsisfun:
; https://www.mathsisfun.com/calculus/differential-equations.html
; https://www.mathsisfun.com/calculus/differential-equations-solution-guide.html
; https://www.mathsisfun.com/calculus/differential-equations-homogeneous.html
