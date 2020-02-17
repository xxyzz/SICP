#lang racket/base

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond [(= trials-remaining 0)
           (/ trials-passed trials)]
          [(experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1))]
          [else
           (iter (- trials-remaining 1) trials-passed)]))
  (iter trials 0))

(define (random-in-range low high)
  (let ([range (- high low)])
    (+ low (* (random) range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (cesaro-test)
    (P (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (* (- x1 x2) (- y1 y2)) (monte-carlo trials cesaro-test)))

(define (predicate x y)
  (<= (+ (* x x) (* y y)) 1))

(estimate-integral predicate -1.0 1.0 -1.0 1.0 1000000)
; 3.139032
