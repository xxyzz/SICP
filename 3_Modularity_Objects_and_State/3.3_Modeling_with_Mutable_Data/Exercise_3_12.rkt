#lang racket/base
; https://docs.racket-lang.org/reference/mpairs.html
; https://blog.racket-lang.org/2007/11/getting-rid-of-set-car-and-set-cdr.html

(define (append x y)
  (if (null? x)
    y
    (mcons (mcar x) (append (mcdr x) y))))

(define (append! x y)
  (set-mcdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (mcdr x)) x (last-pair (mcdr x))))

(define x (mcons 'a (mcons 'b null)))
(define y (mcons 'c (mcons 'd null)))
(define z (append x y))
z
; (a b c d)
; (mcons 'a (mcons 'b (mcons 'c (mcons 'd '()))))

; 🚫 is null
; x -> ⬛⬛->⬛🚫   y -> ⬛⬛->⬛🚫
;      |     |          |     |
;      a     b          c     d

; z -> ⬛⬛->⬛⬛->⬛⬛->⬛🚫
;      |     |    |     |
;      a     b    c     d
(mcdr x)
; (b)
; (mcons 'b '())
(define w (append! x y))
w
; (a b c d)
; (mcons 'a (mcons 'b (mcons 'c (mcons 'd '()))))

;                    y
;                    |
; w, x -> ⬛⬛->⬛⬛->⬛⬛->⬛🚫
;         |     |    |     |
;         a     b    c     d
(mcdr x)
; (b c d)
; (mcons 'b (mcons 'c (mcons 'd '())))
