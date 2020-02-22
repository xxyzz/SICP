#lang racket/base

(define (last-pair x)
  (if (null? (mcdr x)) x (last-pair (mcdr x))))

(define (make-cycle x)
  (set-mcdr! (last-pair x) x)
  x)

; Floyd's cycle-finding algorithm
; https://en.wikipedia.org/wiki/Cycle_detection
(define (next-pair x)
  (if (null? x)
      null
      (mcdr x)))

(define (contain-cycle x)
  (let ([tortoise x]
        [hare (next-pair (next-pair x))])
    (define (iter tortoise hare)
      (cond [(and (null? tortoise) (null? hare)) #f]
            [(eq? tortoise hare) #t]
            [else
             (set! tortoise (next-pair tortoise))
             (set! hare (next-pair (next-pair hare)))
             (iter tortoise hare)]))
    (iter tortoise hare)))

(define z (make-cycle (mcons 'a (mcons 'b (mcons 'c null)))))
(define y (mcons 'a (mcons 'b (mcons 'c null))))

(contain-cycle y)
; #f
(contain-cycle z)
; #t
