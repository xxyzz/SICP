#lang racket/base

(define table (make-hash))
(define (put op type item)
  (hash-set! table (list op type) item))
(define (get op type)
  (hash-ref table (list op type)))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp) (if (same-variable? exp var) 1 0)]
        [else ((get 'deriv (operator exp))
               (operands exp) var)]))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-deriv-package)
  ; internal procedures
  (define (make-sum a1 a2)
    (cond [(=number? a1 0) a2]
          [(=number? a2 0) a1]
          [(and (number? a1) (number? a2)) (+ a1 a2)]
          [else (list '+ a1 a2)]))

  (define (make-product m1 m2)
    (cond [(or (=number? m1 0) (=number? m2 0)) 0]
          [(=number? m1 1) m2]
          [(=number? m2 1) m1]
          [(and (number? m1) (number? m2)) (* m1 m2)]
          [else (list '* m1 m2)]))

  (define (addend s)
    (car s))

  (define (augend s)
    (if (null? (cddr s))
        (cadr s)
        (cons '+ (cdr s))))

  (define (multiplier p)
    (car p))

  (define (multiplicand p)
    (if (null? (cddr p))
        (cadr p)
        (cons '* (cdr p))))

  (define (deriv-sum operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))

  (define (deriv-product operands var)
    (make-sum
     (make-product (multiplier operands)
                   (deriv (multiplicand operands) var))
     (make-product (deriv (multiplier operands) var)
                   (multiplicand operands))))

  (define (make-exponentiation u n)
    (cond [(=number? n 0) 1]
          [(=number? n 1) u]
          [else (list '** u n)]))

  (define (base e)
    (car e))

  (define (exponent e)
    (cadr e))

  (define (deriv-exponent operands var)
    (make-product (exponent operands)
                  (make-product (make-exponentiation (base operands) (sub1 (exponent operands)))
                                (deriv (base operands) var))))

  ; interface to the rest of the system
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-exponent))

(install-deriv-package)
(deriv '(* x y (+ x 3)) 'x)
; '(+ (* x y) (* y (+ x 3)))
(deriv '(** x 4) 'x)
; '(* 4 (** x 3))
