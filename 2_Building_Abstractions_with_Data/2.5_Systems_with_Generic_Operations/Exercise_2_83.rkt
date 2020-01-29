#lang racket/base

(define table (make-hash))
(define (put op type item)
  (hash-set! table (list op type) item))
(define (get op type)
  (hash-ref! table (list op type) null))

(define coercion-table (make-hash))
(define (put-coercion op type item)
  (hash-set! coercion-table (list op type) item))
(define (get-coercion op type)
  (hash-ref! coercion-table (list op type) null))

(define (square x) (* x x))

(define (attach-tag type-tag contents)
  (cond [(and (eq? type-tag 'scheme-number)
              (number? contents))
         contents]
        [else (cons type-tag contents)]))

(define (type-tag datum)
  (cond [(number? datum) 'scheme-number]
        [(pair? datum) (car datum)]
        [else (error "Bad tagged datum: TYPE-TAG" datum)]))

(define (contents datum)
  (cond [(number? datum) datum]
        [(pair? datum) (cdr datum)]
        [else (error "Bad tagged datum: CONTENTS" datum)]))

(define (no-method-error op type-tags)
  (error "No method for these types"
         (list op type-tags)))

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
  (put 'make 'scheme-number (lambda (x) (tag (round x))))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(install-scheme-number-package)

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ([g (gcd n d)])
      (cons (/ n g) (/ d g))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(install-rational-package)

(define (install-real-package)
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'real z))
  (put 'make 'make-real
       (lambda (x) (tag x)))
  'done)

(define (make-real n)
  ((get 'make 'make-real) n))

(install-real-package)

(define (install-rectangular-package)
  ;; internal procedures
  (define (make-from-real-imag x y)
    (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (make-from-mag-ang r a) (cons r a))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)
(install-polar-package)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-complex-package)

(define (install-coercion-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (scheme-number->rational z) (make-rational z 1))
  (put-coercion 'scheme-number
                'rational
                scheme-number->rational)

  (define (rational->real z)
    (make-real (/ (numer z) (denom z))))
  (put-coercion 'rational
                'real
                rational->real)

  (define (real->complex z)
    (let ([type (type-tag z)]
          [content (contents z)])
      (if (eq? type 'scheme-number)
          (make-complex-from-real-imag content 0)
          (make-complex-from-real-imag (/ (numer content) (denom content)) 0))))
  (put-coercion 'real
                'complex
                real->complex)
  'done)

(install-coercion-package)

(define (install-raise-package)
  (define (raise-integer z)
    ((get-coercion 'scheme-number 'rational) z))
  (define (raise-raional z)
    ((get-coercion 'rational 'real) z))
  (define (raise-real z)
    ((get-coercion 'real 'complex) z))
  (put 'raise '(scheme-number) raise-integer)
  (put 'raise '(rational) raise-raional)
  (put 'raise '(real) raise-real)
  'done)

(install-raise-package)

(define (raise z) (apply-generic 'raise z))

(raise (make-scheme-number 1))
; '(rational 1 . 1)

(raise (make-rational 1 2))
; '(real . 1/2)

(raise (make-real 1))
; '(complex rectangular 1 . 0)
