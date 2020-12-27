#lang racket/base

(define (lexical-frame address) (car address))
(define (lexical-var address) (cdr address))

(define (travsersing-env end-proc find-proc env address)
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
  (travsersing-env
   (lambda () (error "Unbound variable" address))
   (lambda (current-pair)
     (if (eq? val '*unassigned*)
         (error "Variable is unassigned at" address)
         (frame-unit-value current-pair)))
   env
   address))

(define (lexical-address-set! address val env)
  (travsersing-env
   (lambda () (error "Unbound variable at: SET!" address))
    (lambda (current-pair) (set-mcdr! current-pair val))
   env
   address))
