#lang racket/base

(define (compile exp target linkage compile-env) ;; ***
  (cond [(self-evaluating? exp)
         (compile-self-evaluating exp target linkage)]
        [(quoted? exp) (compile-quoted exp target linkage)]
        [(variable? exp)
         (compile-variable exp target linkage compile-env)] ;; ***
        [(assignment? exp)
         (compile-assignment exp target linkage compile-env)] ;; ***
        [(definition? exp)
         (compile-definition exp target linkage compile-env)] ;; ***
        [(if? exp) (compile-if exp target linkage compile-env)] ;; ***
        [(lambda? exp) (compile-lambda exp target linkage compile-env)] ;; ***
        [(begin? exp)
         (compile-sequence
          (begin-actions exp) target linkage compile-env)] ;; ***
        [(cond? exp)
         (compile (cond->if exp) target linkage compile-env)] ;; ***
        [(open-code? exp)
         (compile-open-code exp target linkage compile-env)] ;; ***
        [(application? exp)
         (compile-application exp target linkage compile-env)] ;; ***
        [else
         (error "Unknown expression type: COMPILE" exp)]))

(define (find-variable exp compile-env)
  (define (env-loop exp env nth-frame)
    (define (scan vars nth-var)
      (let ([current-var (if (pair? vars) (car vars) null)])
        (cond [(null? current-var)
               (env-loop exp (enclosing-environment env) (add1 nth-frame))]
              [(eq? exp current-var)
               (make-lexical-address nth-frame nth-var)]
              [else
               (scan (cdr vars) (add1 nth-var))])))
    (if (eq? env the-empty-environment)
        (error "not-found" exp)
        (scan (first-frame env) 0)))
  (env-loop exp compile-env 0))

(define (compile-variable exp target linkage compile-env)
  (let ([address (find-variable exp compile-env)])
    (end-with-linkage
     linkage
     (make-instruction-sequence
      '(env)
      (list target)
      `((assign ,target
                (op lexical-address-lookup) ;; ***
                (const ,address) ;; ***
                (reg env)))))))

(define (compile-assignment exp target linkage compile-env)
  (let ([var (assignment-variable exp)]
        [get-value-code
         (compile (assignment-value exp) 'val 'next compile-env)] ;; ***
        [address (find-variable exp compile-env)]) ;; ***
    (end-with-linkage
     linkage
     (preserving
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val)
       (list target)
       `((perform (op lexical-address-set!) ;; ***
                  (const ,address) ;; ***
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))
(define (compile-definition exp target linkage compile-env)
  (let ([var (definition-variable exp)]
        [get-value-code
         (compile (definition-value exp) 'val 'next compile-env)]) ;; ***
    (end-with-linkage
     linkage
     (preserving
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val)
       (list target)
       `((perform (op define-variable!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

(define (compile-if exp target linkage compile-env)
  (let ([t-branch (make-label 'true-branch)]
        [f-branch (make-label 'false-branch)]
        [after-if (make-label 'after-if)])
    (let ([consequent-linkage
           (if (eq? linkage 'next) after-if linkage)])
      (let ([p-code (compile (if-predicate exp) 'val 'next)]
            [c-code
             (compile
              (if-consequent exp) target consequent-linkage compile-env)] ;; ***
            [a-code
             (compile (if-alternative exp) target linkage compile-env)]) ;; ***
        (preserving '(env continue)
                    p-code
                    (append-instruction-sequences
                     (make-instruction-sequence '(val) '()
                                                `((test (op false?) (reg val))
                                                  (branch (label ,f-branch))))
                     (parallel-instruction-sequences
                      (append-instruction-sequences t-branch c-code)
                      (append-instruction-sequences f-branch a-code))
                     after-if))))))

(define (compile-sequence seq target linkage compile-env)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage compile-env) ;; ***
      (preserving '(env continue)
                  (compile (first-exp seq) target 'next compile-env) ;; ***
                  (compile-sequence
                   (rest-exps seq)
                   target
                   linkage
                   compile-env)))) ;; ***

(define (compile-lambda exp target linkage compile-env)
  (let ([proc-entry (make-label 'entry)]
        [after-lambda (make-label 'after-lambda)])
    (let ([lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)])
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage
         lambda-linkage
         (make-instruction-sequence
          '(env)
          (list target)
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env)))))
        (compile-lambda-body exp proc-entry compile-env)) ;; ***
       after-lambda))))

(define (compile-lambda-body exp proc-entry compile-env)
  (let ([formals (lambda-parameters exp)])
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
                                `(,proc-entry
                                  (assign env (op compiled-procedure-env) (reg proc))
                                  (assign env
                                          (op extend-environment)
                                          (const ,formals)
                                          (reg argl)
                                          (reg env))))
     (compile-sequence (lambda-body exp)
                       'val
                       'return
                       (cons formals compile-env))))) ;; ***

(define (compile-application exp target linkage compile-env)
  (let ([proc-code (compile (operator exp) 'proc 'next compile-env)] ;; ***
        [operand-codes
         (map (lambda (operand) (compile operand 'val 'next compile-env)) ;; ***
              (operands exp))])
    (preserving '(env continue)
                proc-code
                (preserving '(proc continue)
                            (construct-arglist operand-codes)
                            (compile-procedure-call target linkage)))))

(define (spread-arguments operand-list compile-env)
  (cons (compile (car operand-list) 'arg1 'next compile-env) ;; ***
        (map (lambda (operand)
               (compile operand 'arg2 'next compile-env)) ;; ***
             (cdr operand-list))))

(define (compile-open-code exp target linkage compile-env)
  (define (compile-rest-open-codes operand-codes)
    (if (null? (cdr operand-codes))
        (preserving '(arg1)
                    (car operand-codes)
                    (make-instruction-sequence
                     '(arg1 arg2)
                     (list target)
                     `((assign ,target
                               (op ,(operator exp))
                               (reg arg1)
                               (reg arg2)))))
        (preserving '(env arg1)
                    (car operand-codes)
                    (append-instruction-sequences
                     (make-instruction-sequence
                      '(arg1 arg2)
                      '(arg1)
                      `((assign arg1
                                (op ,(operator exp))
                                (reg arg1)
                                (reg arg2))))
                     (compile-rest-open-codes
                      (cdr operand-codes))))))
  (let ([operand-codes (spread-arguments (operands exp) compile-env)]) ;; ***
    (end-with-linkage
     linkage
     (preserving
      '(env)
      (car operand-codes)
      (compile-rest-open-codes (cdr operand-codes))))))

;; test
(define test
  (make-machine
   all-regs
   (list (list '+ +)
         (list 'make-compiled-procedure make-compiled-procedure)
         (list 'compiled-procedure-env compiled-procedure-env)
         (list 'extend-environment extend-environment)
         (list 'lexical-address-lookup lexical-address-lookup)
         (list 'list list)
         (list 'cons cons)
         (list 'primitive-procedure? primitive-procedure?)
         (list 'compiled-procedure-entry compiled-procedure-entry)
         (list 'apply-primitive-procedure apply-primitive-procedure)
         (list '* *))
   (statements
    (compile
     '(((lambda (x y)
          (lambda (a b c d e)
            ((lambda (y z) (* x y z))
             (* a b x)
             (+ c d x))))
        3 4)
       1 1 1 1 1)
     'val
     'next
     the-empty-environment))))
(start test)
(get-register-contents test 'val)
;; 45
