#lang racket/base

;; amb code from exercise 4.51
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list 'not not)
        (list 'eq? eq?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'make-hash make-hash)
        (list 'hash-set! hash-set!)
        (list 'hash-has-key? hash-has-key?)
        (list 'hash-ref hash-ref)
        (list 'newline newline)
        (list 'display display)
        (list 'apply apply)
        (list 'eval eval)
        (list 'equal? equal?)
        (list 'append append)
        (list 'pair? pair?)
        (list 'symbol? symbol?)
        (list 'symbol->string symbol->string)
        (list 'string=? string=?)
        (list 'substring substring)
        (list 'string->symbol string->symbol)
        (list 'string-length string-length)
        (list 'string-append string-append)
        (list 'number? number?)
        (list 'number->string number->string)
        (list 'error error)
        (list 'assoc assoc)
        (list 'cadr cadr)
        (list 'caddr caddr)
        (list 'cddr cddr)
        (list 'read read)))

(define (setup-environment)
  (let ([initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)])
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    (define-variable! 'null null initial-env)
    (define-variable! 'user-initial-environment (make-base-namespace) initial-env)
    initial-env))

(define (ensure-fail? exp) (tagged-list? exp 'ensure-fail))
(define (analyze-ensure-fail exp)
  (let ([proc (analyze (cadr exp))])
    (lambda (env succeed fail)
      (proc env
            (lambda (value fail2) (fail)) ;; retry former not current query
            (lambda () (succeed 'ok fail))))))

(define (and? exp) (tagged-list? exp 'and))
(define (analyze-and exp)
  (define (analyze-and-exp e env succeed fail value)
    (if (null? e)
        (succeed value fail)
        (let ([proc (analyze (car e))])
          (proc env
                (lambda (new-value fail2)
                  (if (true? new-value)
                      (analyze-and-exp (cdr e) env succeed fail new-value)
                      (succeed #f fail)))
                fail))))
  (if (null? (cdr exp))
      (lambda (env succeed fail)
        (succeed #t fail))
      (let ([proc (analyze (cadr exp))])
        (lambda (env succeed fail)
          (proc env
                (lambda (value fail2)
                  (if (true? value)
                      (analyze-and-exp (cddr exp) env succeed fail value)
                      (succeed #f fail)))
                fail)))))
(define (or? exp) (tagged-list? exp 'or))
(define (analyze-or exp)
  (define (analyze-or-exp e env succeed fail value)
    (if (null? e)
        (succeed value fail)
        (let ([proc (analyze (car e))])
          (proc env
                (lambda (new-value fail2)
                  (if (true? new-value)
                      (succeed new-value fail)
                      (analyze-or-exp (cdr e) env succeed fail new-value)))
                fail))))
  (if (null? (cdr exp))
      (lambda (env succeed fail)
        (succeed #f fail))
      (let ([proc (analyze (cadr exp))])
        (lambda (env succeed fail)
          (proc env
                (lambda (value fail2)
                  (if (true? value)
                      (succeed value fail)
                      (analyze-or-exp (cddr exp) env succeed fail value)))
                fail)))))

(define (analyze exp)
  (cond [(self-evaluating? exp) (analyze-self-evaluating exp)]
        [(quoted? exp) (analyze-quoted exp)]
        [(variable? exp) (analyze-variable exp)]
        [(assignment? exp) (analyze-assignment exp)]
        [(permanent-set? exp) (analyze-permanent-set exp)]
        [(definition? exp) (analyze-definition exp)]
        [(if? exp) (analyze-if exp)]
        [(lambda? exp) (analyze-lambda exp)]
        [(begin? exp) (analyze-sequence (begin-actions exp))]
        [(cond? exp) (analyze (cond->if exp))]
        [(let? exp) (analyze (let->lambda exp))]
        [(amb? exp) (analyze-amb exp)]
        [(ensure-fail? exp) (analyze-ensure-fail exp)]
        [(and? exp) (analyze-and exp)]
        [(or? exp) (analyze-or exp)]
        [(application? exp) (analyze-application exp)]
        [else (error "Unknown expression type: ANALYZE" exp)]))

(driver-loop)

;; inside amb evaluator loop:
(define (require p) (if (not p) (amb)))
(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

;; query evaluator:
(define table (make-hash))
(define (put op type item)
  (hash-set! table (list op type) item))
(define (get op type)
  (if (hash-has-key? table (list op type))
      (hash-ref table (list op type))
      false)) ;; ***

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))
(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ([q (query-syntax-process (read))])
    (cond [(eq? q 'try-again) (amb)] ;; ***
          [(assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop)]
          [else
           (announce-output output-prompt)
           (display
            (instantiate
                q
                (qeval q null) ;; ***
              (lambda (v f)
                (contract-question-mark v))))
           (query-driver-loop)])))

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond [(var? exp)
           (let ([binding (binding-in-frame exp frame)])
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame)))]
          [(pair? exp)
           (cons (copy (car exp)) (copy (cdr exp)))]
          [else exp]))
  (copy exp))

(define (qeval query frame)
  (let ([qproc (get (type query) 'qeval)])
    (if qproc
        (qproc (contents query) frame)
        (simple-query query frame))))

(define (simple-query query-pattern frame) ;; ***
  (amb
   (find-assertions query-pattern frame)
   (apply-rules query-pattern frame)))

(define (conjoin conjuncts frame) ;; ***
  (if (empty-conjunction? conjuncts)
      frame
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts) frame))))
(put 'and 'qeval conjoin)

(define (disjoin disjuncts frame) ;; ***
  (qeval (an-element-of disjuncts) frame))
(put 'or 'qeval disjoin)

(define (negate operands frame) ;; ***
  (ensure-fail (qeval (negated-query operands)
                      frame))
  frame)
(put 'not 'qeval negate)

(define (lisp-value call frame) ;; ***
  (if (execute
       (instantiate
           call
           frame
         (lambda (v f)
           (error "Unknown pat var: LISP-VALUE" v))))
      frame
      (amb)))
(put 'lisp-value 'qeval lisp-value)

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

(define (always-true ignore frame) frame)
(put 'always-true 'qeval always-true)

(define (find-assertions pattern frame) ;; ***
  (let ([datum (an-element-of (fetch-assertions pattern frame))])
    (check-an-assertion datum pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  (let ([match-result
         (pattern-match query-pat assertion query-frame)])
    (if (eq? match-result 'failed)
        (amb) ;; ***
        match-result)))

(define (pattern-match pat dat frame)
  (cond [(eq? frame 'failed) 'failed]
        [(equal? pat dat) frame]
        [(var? pat) (extend-if-consistent pat dat frame)]
        [(and (pair? pat) (pair? dat))
         (pattern-match
          (cdr pat)
          (cdr dat)
          (pattern-match (car pat) (car dat) frame))]
        [else 'failed]))

(define (extend-if-consistent var dat frame)
  (let ([binding (binding-in-frame var frame)])
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

(define (apply-rules pattern frame) ;; ***
  (let ([rule (an-element-of (fetch-rules pattern frame))])
    (apply-a-rule rule pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ([clean-rule (rename-variables-in rule)])
    (let ([unify-result (unify-match query-pattern
                                     (conclusion clean-rule)
                                     query-frame)])
      (if (eq? unify-result 'failed)
          (amb) ;; ***
          (qeval (rule-body clean-rule)
                 unify-result))))) ;; ***

(define (rename-variables-in rule)
  (let ([rule-application-id (new-rule-application-id)])
    (define (tree-walk exp)
      (cond [(var? exp)
             (make-new-variable exp rule-application-id)]
            [(pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp)))]
            [else exp]))
    (tree-walk rule)))

(define (unify-match p1 p2 frame)
  (cond [(eq? frame 'failed) 'failed]
        [(equal? p1 p2) frame]
        [(var? p1) (extend-if-possible p1 p2 frame)]
        [(var? p2) (extend-if-possible p2 p1 frame)]
        [(and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame))]
        [else 'failed]))

(define (extend-if-possible var val frame)
  (let ([binding (binding-in-frame var frame)])
    (cond [binding
           (unify-match (binding-value binding) val frame)]
          [(var? val)
           (let ([binding (binding-in-frame val frame)])
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame)))]
          [(depends-on? val var frame)
           'failed]
          [else (extend var val frame)])))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond [(var? e)
           (if (equal? var e)
               true ;; ***
               (let ([b (binding-in-frame e frame)])
                 (if b
                     (tree-walk (binding-value b))
                     false)))] ;; ***
          [(pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e)))]
          [else false])) ;; ***
  (tree-walk exp))

(define THE-ASSERTIONS null) ;; ***
(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))
(define (get-all-assertions) THE-ASSERTIONS)
(define (get-indexed-assertions pattern)
  (get-list (index-key-of pattern) 'assertion-list)) ;; ***

(define (get-list key1 key2) ;; ***
  (let ([s (get key1 key2)])
    (if s s null))) ;; ***

(define THE-RULES null)
(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))
(define (get-all-rules) THE-RULES)
(define (get-indexed-rules pattern) ;; ***
  (append
   (get-list (index-key-of pattern) 'rule-list)
   (get-list '? 'rule-list)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ([old-assertions THE-ASSERTIONS])
    (permanent-set! THE-ASSERTIONS
          (cons assertion old-assertions)) ;; ***
    'ok))
(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ([old-rules THE-RULES])
    (permanent-set! THE-RULES (cons rule old-rules)) ;; ***
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)                  ;; ***
    (let ([key (index-key-of assertion)])
      (let ([current-assertion-list           ;; ***
             (get-list key 'assertion-list)]) ;; ***
        (put key
             'assertion-list
             (cons assertion ;; ***
                   current-assertion-list))))))
(define (store-rule-in-index rule)
  (let ([pattern (conclusion rule)])
    (if (indexable? pattern)               ;; ***
      (let ([key (index-key-of pattern)])
        (let ([current-rule-list           ;; ***
               (get-list key 'rule-list)]) ;; ***
          (put key
               'rule-list
               (cons rule ;; ***
                     current-rule-list)))))))

;; rest query evaluator code(exercise 4.72) unchanged
;; except these two and without steams code
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false)) ;; ***
(define (new-rule-application-id)
  (permanent-set! rule-counter (+ 1 rule-counter)) ;; ***
  rule-counter)

;; add data: exercise 4.55
(query-driver-loop)

;; it won't interleave results
;; run the test at the bottom of exercise 4.72
;; it will always return the same result
