#lang racket/base
(require racket/math)

(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond [(eq? op 'real-part) (* mag (cos ang))]
          [(eq? op 'imag-part) (* mag (sin ang))]
          [(eq? op 'magnitude) mag]
          [(eq? op 'angle) ang]
          [else (error "Unknown op: MAKE-FROM-MAG-ANG" op)]))
  dispatch)
  
(define (apply-generic op arg) (arg op))

(define test (make-from-mag-ang 1 pi))
(apply-generic 'real-part test)
; -1.0
(apply-generic 'imag-part test)
; 1.2246467991473532e-16   that's 0
(apply-generic 'magnitude test)
; 1
(apply-generic 'angle test)
; 3.141592653589793
