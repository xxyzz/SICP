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
        [else null]))

(define (contents datum)
  (cond [(number? datum) datum]
        [(pair? datum) (cdr datum)]
        [else (error "Bad tagged datum: CONTENTS" datum)]))

(define (apply-generic op . args)
  (let ([type-tags (map type-tag args)])
    (let ([proc (get op type-tags)])
      (if (not (null? proc))
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))

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

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-zero?-package)
  (define (numer x) (car x))
  (define (=zero?-ordinary x) (zero? x))
  (define (=zero?-rational x) (zero? (numer x)))
  (put '=zero? '(scheme-number) =zero?-ordinary)
  (put '=zero? '(rational) =zero?-rational))

(install-zero?-package)

(define (=zero? x) (apply-generic '=zero? x))

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x)
    (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))

  (define (add-terms L1 L2)
    (cond [(empty-termlist? L1) L2]
          [(empty-termlist? L2) L1]
          [else
           (let ([t1 (first-term L1)]
                 [t2 (first-term L2)])
             (cond [(> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2))]
                   [(< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2)))]
                   [else (adjoin-term
                          (make-term (order t1)
                                     (add (coeff t1) (coeff t2)))
                          (add-terms (rest-terms L1) (rest-terms L2)))]))]))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        L1
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        L
        (let ([t2 (first-term L)])
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (=zero?-poly x)
    (define (check-terms terms)
      (or (empty-termlist? terms) ; never happens, add-terms and mul-term-by-all-terms already checked
          (and (=zero? (coeff (first-term terms)))
               (check-terms (rest-terms terms)))))
    (check-terms (term-list x)))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) =zero?-poly)
  'done)

(install-polynomial-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define poly-a (make-polynomial 'x '((100 1) (2 2) (0 1))))
; '(polynomial x (100 1) (2 2) (0 1))
; x^100 + 2x^2 + 1
(add poly-a poly-a)
; '(polynomial x (100 2) (2 4) (0 2))
; 2x^100 + 4x^2 + 2

(define poly-b (make-polynomial 'y '((1 1) (0 1))))
; '(polynomial y (1 1) (0 1))
; y + 1

(define poly-c (make-polynomial 'x (list (list 2 1) (list 1 poly-b) (list 0 5))))
; '(polynomial x (2 1) (1 (polynomial y (1 1) (0 1))) (0 5))
; x^2 + (y + 1)x + 5

(define poly-d (make-polynomial 'x '((2 1) (1 2) (0 1))))
; '(polynomial x (2 1) (1 2) (0 1))
; x^2 + 2x + 1

(add poly-c poly-c)
; '(polynomial x (2 2) (1 (polynomial y (1 2) (0 2))) (0 10))
; 2x^2 + (2y + 2)x + 10
