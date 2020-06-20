#lang racket/base

(define (mmap proc . args)
  (if (null? (car args))
      null
      (mcons
       (apply proc (map mcar args))
       (apply mmap
              (cons proc (map mcdr args))))))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (mmap mcons variables values))
(define (frame-variables frame) (mmap mcar frame))
(define (frame-values frame) (mmap mcdr frame))
(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (mcons (mcons var val) (mcar frame))))
(define (frame-unit-variable unit) (mcar unit))
(define (frame-unit-value unit) (mcdr unit))

(define (travsersing-env end-frame-proc find-proc end-env-proc env var)
  (define (env-loop env)
    (define (scan pairs)
      (let ([current-pair (mcar pairs)])
        (cond [(null? current-pair)
               (end-frame-proc env)]
              [(eq? var (frame-unit-variable current-pair))
               (find-proc current-pair)]
              [else (scan (mcdr pairs))])))
    (if (eq? env the-empty-environment)
        (end-env-proc var)
        (let ([frame (first-frame env)])
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (travsersing-env
   (lambda (env) (set-variable-value! var val (enclosing-environment env)))
   (lambda (current-pair) (set-mcdr! current-pair val))
   (lambda (var) (error "Unbound variable: SET!" var))
   env
   var))

(define (define-variable! var val env)
  (travsersing-env
   (lambda (env) (add-binding-to-frame! var val (first-frame env)))
   (lambda (current-pair) (set-mcdr! current-pair val))
   (lambda (var) (error "Empty environment" var))
   env
   var))

(define (lookup-variable-value var env)
  (travsersing-env
   (lambda (env) (lookup-variable-value var (enclosing-environment env)))
   (lambda (current-pair) (frame-unit-value current-pair))
   (lambda (var) (error "Unbound variable" var))
   env
   var))