#lang racket/base

(define (identity x) x)

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
    (define (iter-types type-list)
      (if (null? type-list)
          (no-method-error op type-tags) ; all types are tired
          (let* ([try-type (car type-list)]
                 [proc-list (map (lambda (arg)
                                   (if (eq? (type-tag arg) try-type)
                                       identity
                                       (get-coercion (type-tag arg) try-type)))
                                 args)])
            (if (null? (filter null? proc-list))
                (let* ([converted-args (map (lambda (proc arg) (proc arg))
                                            proc-list args)]
                       [types (map type-tag converted-args)]
                       [proc (get op types)])
                  (if (not (null? proc))
                      (apply proc (map contents converted-args))
                      (iter-types (cdr type-list)))) ; no suitable coercion procedure
                (iter-types (cdr type-list))))))     ; can't convert to the same type
    (let ([proc (get op type-tags)])
      (if (not (null? proc))
          (apply proc (map contents args))
          (if (null? (filter (lambda (type)
                               (not (eq? type (car type-tags))))
                             type-tags))
              (no-method-error op type-tags) ; same type
              (iter-types type-tags))))))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'make 'scheme-number (lambda (x) (tag x)))
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
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational rational)
       (lambda (x y z) (tag (add-rat (add-rat x y) z))))
  (put 'add '(rational rational scheme-number)
       (lambda (x y z)
         (display "Mixed-type procedure\n")
         (tag (add-rat (add-rat x y) (make-rat z 1)))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(install-rational-package)

(define (add x y z) (apply-generic 'add x y z))

(define (install-coercion-package)
  (define (scheme-number->rational z) (make-rational z 1))
  (put-coercion 'scheme-number
                'rational
                scheme-number->rational)
  'done)

(install-coercion-package)

(define a (make-scheme-number 1))
(define b (make-rational 1 2))
(add a b a)
; '(rational 5 . 2)
(add b b a)
; Mixed-type procedure
; '(rational 2 . 1)
