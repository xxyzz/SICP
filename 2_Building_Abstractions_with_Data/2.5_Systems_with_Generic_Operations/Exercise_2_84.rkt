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

(define (raise z)
  (let ([raise-proc (get 'raise (type-tag z))])
    (if (null? raise-proc)
        null
        (raise-proc (contents z)))))

(define (try-raise arg target-type)
  (let ([converted-arg (raise arg)])
    (cond [(null? converted-arg) null] ; can't raise any more
          [(eq? target-type (type-tag converted-arg)) converted-arg]
          [else (try-raise converted-arg target-type)])))

(define (no-method-error op type-tags)
  (error "No method for these types"
         (list op type-tags)))

(define (apply-generic op . args)
  (let ([type-tags (map type-tag args)])
    (define (iter-types type-list)
      (if (null? type-list)
          (no-method-error op type-tags) ; all types are tired
          (let* ([try-type (car type-list)]
                 [converted-args (map (lambda (arg)
                                        (if (eq? (type-tag arg) try-type)
                                            arg
                                            (try-raise arg try-type)))
                                      args)])
            (if (null? (filter null? converted-args))
                (let* ([types (map type-tag converted-args)]
                       [proc (get op types)])
                  (if (null? proc)
                      (iter-types (cdr type-list))   ; no suitable coercion procedure
                      (apply proc (map contents converted-args))))
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
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(install-rational-package)

(define (install-real-package)
  ;; imported procedures from scheme number and rational packages
  (define (make-from-integer n)
    ((get 'make 'scheme-number) n))
  (define (make-from-rational n d)
    ((get 'make 'rational) n d))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'real z))
  (put 'make 'make-real-from-integer
       (lambda (n) (tag (make-from-integer n))))
  (put 'make 'make-real-from-rational
       (lambda (n d) (tag (make-from-rational n d))))
  'done)

(define (make-real-from-integer n)
  ((get 'make 'make-real-from-integer) n))
(define (make-real-from-rational n d)
  ((get 'make 'make-real-from-rational) n d))

(install-real-package)

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y)
    (cons x y))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)
(install-polar-package)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'add '(complex complex complex complex)
       (lambda (z1 z2 z3 z4) (tag (add-complex (add-complex (add-complex z1 z2) z3) z4))))
  (put 'real-part '(complex) real-part) 
  (put 'imag-part '(complex) imag-part) 
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-complex-package)

(define (add x1 x2 x3 x4) (apply-generic 'add x1 x2 x3 x4))

(define (install-coercion-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (scheme-number->rational z) (make-rational z 1))
  (put-coercion 'scheme-number
                'rational
                scheme-number->rational)

  (define (rational->real z)
    (make-real-from-rational (numer z) (denom z)))
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
  (put 'raise 'scheme-number raise-integer)
  (put 'raise 'rational raise-raional)
  (put 'raise 'real raise-real)
  'done)

(install-raise-package)

(define a (make-scheme-number 1))
(define b (make-rational 1 2))
(define c (make-complex-from-real-imag 1 0))
(define d (make-complex-from-mag-ang 1 0))
(add a b c d)
; '(complex rectangular 3 1/2 . 0)
