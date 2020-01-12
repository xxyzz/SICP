#lang racket/base

; a:
(define (variable? x)
    (symbol? x))

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
    (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
    (cond [(=number? a1 0) a2]
          [(=number? a2 0) a1]
          [(and (number? a1) (number? a2)) (+ a1 a2)]
          [else (list a1 '+ a2)]))

(define (make-product m1 m2)
    (cond [(or (=number? m1 0) (=number? m2 0)) 0]
          [(=number? m1 1) m2]
          [(=number? m2 1) m1]
          [(and (number? m1) (number? m2)) (* m1 m2)]
          [else (list m1 '* m2)]))

(define (sum? x)
    (and (pair? x) (eq? (cadr x) '+)))

(define (addend s)
    (car s))

(define (augend s)
    (caddr s))

(define (product? x)
    (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p)
    (car p))

(define (multiplicand p)
    (caddr p))

(define (deriv exp var)
    (cond [(number? exp) 0]
          [(variable? exp)
              (if (same-variable? exp var) 1 0)]
          [(sum? exp) (make-sum (deriv (addend exp) var)
                                (deriv (augend exp) var))]
          [(product? exp)
              (make-sum
                  (make-product (multiplier exp)
                                (deriv (multiplicand exp) var))
                  (make-product (deriv (multiplier exp) var)
                                (multiplicand exp)))]
          [else (error "unknown expression type: DERIV" exp)]))

(deriv '(x + (3 * (x + (y + 2)))) 'x)
; 4

; b:
(define (memq item x)
    (cond [(null? x) #f]
          [(eq? item (car x)) x]
          [else (memq item (cdr x))]))

(define (item-or-list x)
    (if (null? (cdr x))
        (car x) ; just one item
        x))     ; list

; check list contains at least one '+
(define (sum? x)
    (and (pair? x) (memq '+ x)))

; get items before the first '+
(define (addend s)
    (define (iter result li)
        (if (eq? (car li) '+)
            (item-or-list result)
            (iter (append result (list (car li))) (cdr li))))
    (iter null s))

(define (augend s)
    (item-or-list (cdr (memq '+ s)))) ; remove '+

(define (multiplicand p)
    (item-or-list (cddr p)))

(deriv '(x + 3 * (x + y + 2)) 'x)
; 4

(deriv '(x * y + x * y + x) 'x)
; '(y + (y + 1))
