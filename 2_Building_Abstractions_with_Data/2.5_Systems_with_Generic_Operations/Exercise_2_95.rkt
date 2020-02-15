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
  (put 'invert '(scheme-number)
       (lambda (x) (tag (- x))))
  (put 'greatest-common-divisor '(scheme-number scheme-number)
       (lambda (x y) (tag (gcd x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(install-scheme-number-package)

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (invert x) (apply-generic 'invert x))
(define (greatest-common-divisor x y) (apply-generic 'greatest-common-divisor x y))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ([g (greatest-common-divisor n d)])
      (cons (div n g) (div d g))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(install-rational-package)

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
                   [else
                    (if (zero? (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1) (rest-terms L2))
                        (adjoin-term
                         (make-term (order t1)
                                    (add (coeff t1) (coeff t2)))
                         (add-terms (rest-terms L1) (rest-terms L2))))]))]))

  (define (sub-poly p1 p2)
    (add-poly p1 (invert-poly p2)))

  (define (invert-terms terms)
    (if (empty-termlist? terms)
        terms
        (let ([current-term (first-term terms)])
          (adjoin-term (make-term (order current-term) (invert (coeff current-term)))
                       (invert-terms (rest-terms terms))))))

  (define (invert-poly p)
    (make-poly (variable p) (invert-terms (term-list p))))

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

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let* ([div-result-terms (div-terms (term-list p1) (term-list p2))]
               [quotient-term-list (car div-result-terms)]
               [remainder-term-list (cadr div-result-terms)])
          (list (make-poly (variable p1) quotient-term-list)
                (make-poly (variable p1) remainder-term-list)))
        (error "Polys not in same var: DIV-POLY" (list p1 p2))))

  (define (div-terms L1 L2)
    (display "div-term L1 ")
    (display L1)
    (display "\n")
    (display "div-term L2 ")
    (display L2)
    (display "\n\n")
    (if (empty-termlist? L1)
        (list L1 L1)
        (let* ([t1 (first-term L1)]
               [t2 (first-term L2)]
               [t1-order (order t1)]
               [t2-order (order t2)])
          (if (> t2-order t1-order)
              (list (the-empty-termlist) L1)
              (let* ([new-c (div (coeff t1) (coeff t2))]
                     [new-o (- t1-order t2-order)]
                     [first-term (make-term new-o new-c)]
                     [rest-of-result
                      (div-terms
                       (add-terms L1
                                  (invert-terms
                                   (mul-term-by-all-terms first-term L2)))
                       L2)])
                (list (adjoin-term first-term (car rest-of-result))
                      (cadr rest-of-result)))))))

  (define (gcd-terms a b)
    (display "gcd-terms a ")
    (display a)
    (display "\n")
    (display "gcd-terms b ")
    (display b)
    (display "\n\n")
    (if (empty-termlist? b)
        a
        (gcd-terms b (remainder-terms a b))))

  (define (remainder-terms a b)
    (cadr (div-terms a b)))

  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (gcd-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var: GCD-POLY" (list p1 p2))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) =zero?-poly)
  (put 'invert '(polynomial)
       (lambda (p) (tag (invert-poly p))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2)
         (let* ([result (div-poly p1 p2)]
                [quotient (car result)]
                [remainder (cadr result)])
           (if (=zero?-poly remainder)
               (tag quotient)
               (list (tag (car result)) (tag (cadr result)))))))
  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  'done)

(install-polynomial-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define p1 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
(define p2 (make-polynomial 'x '((2 11) (0 7))))
(define p3 (make-polynomial 'x '((1 13) (0 5))))
(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

(greatest-common-divisor q1 q2)
; gcd-terms a ((4 11) (3 -22) (2 18) (1 -14) (0 7))
; gcd-terms b ((3 13) (2 -21) (1 3) (0 5))

; div-term L1 ((4 11) (3 -22) (2 18) (1 -14) (0 7))
; div-term L2 ((3 13) (2 -21) (1 3) (0 5))

; div-term L1 ((3 -55/13) (2 201/13) (1 -237/13) (0 7))
; div-term L2 ((3 13) (2 -21) (1 3) (0 5))

; div-term L1 ((2 1458/169) (1 -2916/169) (0 1458/169))
; div-term L2 ((3 13) (2 -21) (1 3) (0 5))

; gcd-terms a ((3 13) (2 -21) (1 3) (0 5))
; gcd-terms b ((2 1458/169) (1 -2916/169) (0 1458/169))

; div-term L1 ((3 13) (2 -21) (1 3) (0 5))
; div-term L2 ((2 1458/169) (1 -2916/169) (0 1458/169))

; div-term L1 ((2 5) (1 -10) (0 5))
; div-term L2 ((2 1458/169) (1 -2916/169) (0 1458/169))

; div-term L1 ()
; div-term L2 ((2 1458/169) (1 -2916/169) (0 1458/169))

; gcd-terms a ((2 1458/169) (1 -2916/169) (0 1458/169))
; gcd-terms b ()

; '(polynomial x (2 8 106/169) (1 -17 43/169) (0 8 106/169))

; 2.96 b:
