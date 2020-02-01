#lang racket/base
(require racket/math) ; pi

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
        [else (error "Bad tagged datum: TYPE-TAG" datum)]))

(define (contents datum)
  (cond [(number? datum) datum]
        [(pair? datum) (cdr datum)]
        [else (error "Bad tagged datum: CONTENTS" datum)]))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (magnitude z)
    (square-root (add (square (real-part z)) (square (imag-part z)))))
  (define (angle z)
    (arctan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (sine a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (square-root (add (square x) (square y)))
          (arctan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)
(install-polar-package)

(define (apply-generic op . args)
  (let ([type-tags (map type-tag args)])
    (let ([proc (get op type-tags)])
      (if (not (null? proc))
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

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
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
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

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  (put 'real-part '(complex) real-part) 
  (put 'imag-part '(complex) imag-part) 
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-complex-package)

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

(define (square x)
  (let* ([type (type-tag x)]
         [content (contents x)]
         [proc (get 'mul (list type type))])
    (if (not (null? proc))
        (proc content content)
        0)))

(define (install-generic-operation-package)
  (define (rational-value x)
    (/ (car x) (cdr x)))
  (define (sine-scheme-number x)
    (sin x))
  (define (sine-rational x)
    (make-rational (sin (rational-value x)) 1))
  (define (cosine-scheme-number x)
    (cos x))
  (define (cosine-rational x)
    (make-rational (cos (rational-value x)) 1))
  (define (arctangent-scheme-number x y)
    (atan x y))
  (define (arctangent-rational x y)
    (make-rational (atan (rational-value x) (rational-value y)) 1))
  (define (square-root-scheme-number x)
    (sqrt x))
  (define (square-root-rational x)
    (make-rational (sqrt (rational-value x)) 1))
  (put 'sine '(scheme-number) sine-scheme-number)
  (put 'sine '(rational) sine-rational)
  (put 'cosine '(scheme-number) cosine-scheme-number)
  (put 'cosine '(rational) cosine-rational)
  (put 'arctan '(scheme-number scheme-number) arctangent-scheme-number)
  (put 'arctan '(rational rational) arctangent-rational)
  (put 'square-root '(scheme-number) square-root-scheme-number)
  (put 'square-root '(rational) square-root-rational))

(install-generic-operation-package)

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (arctan x y) (apply-generic 'arctan x y))
(define (square-root x) (apply-generic 'square-root x))

(define a (make-scheme-number 1))
; 1
(define b (make-scheme-number 0))
; 0
(define c (make-rational 1 2))
; '(rational 1 . 2)
(define d (make-complex-from-real-imag a a))
; '(complex rectangular 1 . 1)
(define e (make-complex-from-mag-ang a b))
; '(complex polar 1 . 0)
(define f (make-complex-from-real-imag c c))
; '(complex rectangular (rational 1 . 2) rational 1 . 2)
(define g (make-complex-from-mag-ang (make-rational 1 1) (make-rational 0 1)))
; ''(complex polar (rational 1 . 1) rational 0 . 1)

(add d d)
; '(complex rectangular 2 . 2)

(real-part d)
; 1
(imag-part d)
; 1
(magnitude d)
; 1.4142135623730951 = √2
(angle d)
; 0.7853981633974483 = π / 4

(real-part e)
; 1
(imag-part e)
; 0
(magnitude e)
; 1
(angle e)
; 0

(real-part f)
; '(rational 1 . 2)
(imag-part f)
; '(rational 1 . 2)
(magnitude f)
; '(rational 6369051672525773.0 . 9007199254740992.0)
; 6369051672525773.0 / 9007199254740992.0 = 0.707106781186548 = √2 / 2
(angle f)
; '(rational 884279719003555.0 . 1125899906842624.0)
; 884279719003555.0 / 1125899906842624.0 = 0.785398163397448 = π / 4

(real-part g)
; '(rational 1 . 1)
(imag-part g)
; '(rational 0 . 1)
(magnitude g)
; '(rational 1 . 1)
(angle g)
; '(rational 0 . 1)

(add d d)
; '(complex rectangular 2 . 2)

(add e e)
; '(complex rectangular 2 . 0)

(add f f)
; '(complex rectangular (rational 1 . 1) rational 1 . 1)

(add g g)
; '(complex rectangular (rational 2 . 1) rational 0 . 1)
