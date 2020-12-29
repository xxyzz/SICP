#lang racket/base

(define (make-lexical-address nth-frame nth-var)
  (cons nth-frame nth-var))
(define (lexical-frame address) (car address))
(define (lexical-var address) (cdr address))

(define (travsersing-compile-env end-proc find-proc env address)
  (define (env-loop env nth-frame nth-var)
    (define (scan pairs nth-pair)
      (let ([current-pair (if (mpair? pairs) (mcar pairs) null)])
        (cond [(null? current-pair)
               (end-proc)]
              [(zero? nth-pair)
               (find-proc current-pair)]
              [else
               (scan (mcdr pairs) (sub1 nth-pair))])))
    (cond [(eq? env the-empty-environment)
           (end-proc)]
          [(zero? nth-frame)
           (scan (first-frame env) nth-var)]
          [else
           (env-loop (enclosing-environment env)
                     (sub1 nth-frame)
                     nth-var)]))
  (env-loop env
            (lexical-frame address)
            (lexical-var address)))

(define (lexical-address-lookup address env)
  (travsersing-compile-env
   (lambda () (error "Unbound variable" address))
   (lambda (current-pair)
     (let ([value (frame-unit-value current-pair)])
       (if (eq? value '*unassigned*)
           (error "Variable is unassigned at" address)
           value)))
   env
   address))

(define (lexical-address-set! address val env)
  (travsersing-compile-env
   (lambda () (error "Unbound variable at: SET!" address))
    (lambda (current-pair) (set-mcdr! current-pair val))
   env
   address))
