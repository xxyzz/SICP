#lang racket/base

(define table (make-hash))
(define (put op type item)
  (hash-set! table (list op type) item))
(define (get op type)
  (hash-ref! table (list op type) null))

(define (attach-tag type-tag contents)
  (cond [(and (eq? type-tag 'scheme-number)
              (number? contents))
         contents]
        [else (cons type-tag contents)]))

(define (type-tag datum)
  (cond [(number? datum) 'scheme-number]
        [(pair? datum) (car datum)]
        [else (error "Bad tagged datum: TYPE-TAG" datum)])

(define (contents datum)
  (cond [(number? datum) datum]
        [(pair? datum) (cdr datum)]
        [else (error "Bad tagged datum: CONTENTS" datum)])

(define (apply-generic op . args)
  (let ([type-tags (map type-tag args)])
    (let ([proc (get op type-tags)])
      (if (not (null? proc))
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(install-scheme-number-package)

(define a (make-scheme-number 1))
(define b (make-scheme-number 2))
a
; 1
b
; 2
(add a b)
; 3
(sub a b)
; -1
(mul a b)
; 2
(div a b)
; 1/2
