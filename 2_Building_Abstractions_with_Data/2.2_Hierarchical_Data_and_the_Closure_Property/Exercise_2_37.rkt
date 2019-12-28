#lang racket/base

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        null
        (cons (accumulate op init (map (lambda (x) (car x)) seqs))
              (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

; 3BLUE1BROWN: Dot products and duality | Essence of linear algebra, chapter 9            
; https://www.youtube.com/watch?v=LyGKycYT2v0&list=PLZHQObOWTQDPD3MizzM2xVFitgF8hE_ab&index=9
(define (dot-product v w)
      (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
    (map (lambda (m-row)
            (dot-product v m-row))
         m))

(define (transpose mat)
    (accumulate-n cons null mat))
; wow

(define (matrix-*-matrix m n)
    (let ([cols (transpose n)])
        (map (lambda (m-row)
             (map (lambda (n-col)
                 (dot-product m-row n-col))
                 cols))
             m)))

(define v (list 1 2))
(define m (list (list 1 2) (list 3 4)))
(dot-product v v)
; 5
(matrix-*-vector m v)
; '(5 11)
(transpose m)
; '((1 3) (2 4))
(matrix-*-matrix m m)
; '((7 10) (15 22))
