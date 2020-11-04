#lang racket/base

(define (make-filter-promise qproc query vars)
  (list 'filter-promise qproc query vars))

(define (filter-promise? frame)
  (tagged-list? frame 'filter-promise))

(define (filter-promise-qproc promise)
  (cadr promise))

(define (filter-promise-query promise)
  (caddr promise))

(define (filter-promise-vars promise)
  (cadddr promise))

(define (force-filter-promise promise frame)
  (let ([qproc (filter-promise-qproc promise)]
        [query (filter-promise-query promise)])
    (qproc query (singleton-stream frame) null)))

(define (remove-filter-promise frame-stream)
  (stream-filter
   (lambda (frame)
     (not (filter-promise? frame)))
   frame-stream))

(define (filter-already-bound? exp frame-stream)
  (stream-ormap
   (lambda (frame)
     (filter-bound-in-frame? exp frame))
   (remove-filter-promise frame-stream)))

(define (find-in-query? var query)
  (cond [(not (pair? query)) #f]
        [(equal? var query)]
        [else (or (find-in-query? var (car query))
                  (find-in-query? var (cdr query)))]))

(define (duplicated-var? var query)
  (> (foldl
      (lambda (exp result)
        (+ (if (find-in-query? var exp)
               1
               0)
           result))
      0
      query)
     1))

(define (not-filter-vars query full-query)
  (define (iter exp vars)
    (cond [(null? exp) vars]
          [(var? exp)
           (if (duplicated-var? exp full-query)
               (cons exp vars)
               vars)]
          [(pair? exp)
           (iter (cdr exp)
                 (iter (car exp) vars))]
          [else vars]))
  (iter query null))

(define (create-negate-promise operands frame-stream full-query)
  (let ([vars (not-filter-vars operands full-query)])
    (if (filter-already-bound? vars frame-stream)
        (negate operands frame-stream full-query)
        (stream-cons (make-filter-promise negate operands vars)
                     frame-stream))))
(put 'not 'qeval create-negate-promise) ;; ***

(define (create-lisp-value-promise call frame-stream full-query)
  (if (filter-already-bound? call frame-stream)
      (lisp-value call frame-stream null)
      (stream-cons (make-filter-promise lisp-value call null)
                   frame-stream)))
(put 'lisp-value 'qeval create-lisp-value-promise) ;; ***

(define (filter-bound-in-frame? query frame)
  (define (bound? exp)
    (cond [(var? exp)
           (let ([binding (binding-in-frame exp frame)])
             (if binding
                 (bound? (binding-value binding))
                 #f))]
          [(pair? exp)
           (and (bound? (car exp)) (bound? (cdr exp)))]
          [else #t]))
  (and (not (null? frame))
       (andmap bound? query)))

(define (frame-passed-filter? frame frame-stream)
  (stream-andmap
   (lambda (filter-promise)
     (let ([exp
            (if (null? (filter-promise-vars filter-promise))
                (filter-promise-query filter-promise)
                (filter-promise-vars filter-promise))])
       (if (filter-bound-in-frame? exp frame)
           (not (stream-empty? (force-filter-promise filter-promise frame)))
           #t)))
   (stream-filter
    filter-promise?
    frame-stream)))

(define (extend variable value frame frame-stream)
  (let ([extened-frame (cons (make-binding variable value) frame)])
    (if (frame-passed-filter? extened-frame frame-stream) ;; ***
        extened-frame
        'failed)))

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ([q (query-syntax-process (read))])
    (cond [(assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop)]
          [else
           (announce-output output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate
                   q
                   frame
                 (lambda (v f)
                   (contract-question-mark v))))
             (remove-filter-promise (qeval q
                                           (singleton-stream '())
                                           q)))) ;; ***
           (query-driver-loop)])))

;; pass frame-stream and full-query
(define (qeval query frame-stream full-query)
  (let ([qproc (get (type query) 'qeval)])
    (if qproc
        (qproc (contents query) frame-stream full-query) ;; ***
        (simple-query query frame-stream))))

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (filter-promise? frame) ;; ***
         (singleton-stream frame)
         (stream-append-delayed
          (find-assertions query-pattern frame frame-stream)       ;; ***
          (delay (apply-rules query-pattern frame frame-stream))))) ;; ***
   frame-stream))

(define (conjoin conjuncts frame-stream full-query)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts) frame-stream full-query) ;; ***
               full-query))) ;; ***

(define (disjoin disjuncts frame-stream full-query)
  (if (empty-disjunction? disjuncts)
      empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream full-query) ;; ***
       (delay (disjoin (rest-disjuncts disjuncts) frame-stream full-query))))) ;; ***

(define (negate operands frame-stream full-query)
  (stream-flatmap
   (lambda (frame)
     (if (stream-empty?
          (qeval (negated-query operands)
                 (singleton-stream frame)
                 full-query)) ;; ***
         (singleton-stream frame)
         empty-stream))
   frame-stream))

(define (lisp-value call frame-stream full-query) ;; ***
  (stream-flatmap
   (lambda (frame)
     (if (execute
          (instantiate
              call
              frame
            (lambda (v f)
              (error "Unknown pat var: LISP-VALUE" v))))
         (singleton-stream frame)
         empty-stream))
   frame-stream))

(define (always-true ignore frame-stream full-query) frame-stream)

(define (find-assertions pattern frame frame-stream)
  (stream-flatmap
   (lambda (datum) (check-an-assertion datum pattern frame frame-stream)) ;; ***
   (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame frame-stream)
  (let ([match-result
         (pattern-match query-pat assertion query-frame frame-stream)]) ;; ***
    (if (eq? match-result 'failed)
        empty-stream
        (singleton-stream match-result))))

(define (pattern-match pat dat frame frame-stream)
  (cond [(eq? frame 'failed) 'failed]
        [(equal? pat dat) frame]
        [(var? pat) (extend-if-consistent pat dat frame frame-stream)] ;; ***
        [(and (pair? pat) (pair? dat))
         (pattern-match
          (cdr pat)
          (cdr dat)
          (pattern-match (car pat) (car dat) frame frame-stream) ;; ***
          frame-stream)] ;; ***
        [else 'failed]))

(define (extend-if-consistent var dat frame frame-stream)
  (let ([binding (binding-in-frame var frame)])
    (if binding
        (pattern-match (binding-value binding) dat frame frame-stream) ;; ***
        (extend var dat frame frame-stream)))) ;; ***

(define (apply-rules pattern frame frame-stream)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame frame-stream)) ;; ***
                  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame frame-stream)
  (let ([clean-rule (rename-variables-in rule)])
    (let ([unify-result (unify-match query-pattern
                                     (conclusion clean-rule)
                                     query-frame
                                     frame-stream)]) ;; ***
      (if (eq? unify-result 'failed)
          empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result)
                 (rule-body clean-rule)))))) ;; ***

(define (unify-match p1 p2 frame frame-stream)
  (cond [(eq? frame 'failed) 'failed]
        [(equal? p1 p2) frame]
        [(var? p1) (extend-if-possible p1 p2 frame frame-stream)] ;; ***
        [(var? p2) (extend-if-possible p2 p1 frame frame-stream)] ;; ***
        [(and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame
                                   frame-stream) ;; ***
                      frame-stream)] ;; ***
        [else 'failed]))

(define (extend-if-possible var val frame frame-stream)
  (let ([binding (binding-in-frame var frame)])
    (cond [binding
           (unify-match (binding-value binding) val frame frame-stream)] ;; ***
          [(var? val)
           (let ([binding (binding-in-frame val frame)])
             (if binding
                 (unify-match
                  var (binding-value binding) frame frame-stream) ;; ***
                 (extend var val frame frame-stream)))] ;; ***
          [(depends-on? val var frame)
           'failed]
          [else (extend var val frame frame-stream)]))) ;; ***

;; test from 4.4.3
(and (not (job ?x (computer programmer)))
     (supervisor ?x ?y))
;;; Query results:
(and (not (job (Aull DeWitt) (computer programmer))) (supervisor (Aull DeWitt) (Warbucks Oliver)))
(and (not (job (Cratchet Robert) (computer programmer))) (supervisor (Cratchet Robert) (Scrooge Eben)))
(and (not (job (Scrooge Eben) (computer programmer))) (supervisor (Scrooge Eben) (Warbucks Oliver)))
(and (not (job (Bitdiddle Ben) (computer programmer))) (supervisor (Bitdiddle Ben) (Warbucks Oliver)))
(and (not (job (Reasoner Louis) (computer programmer))) (supervisor (Reasoner Louis) (Hacker Alyssa P)))
(and (not (job (Tweakit Lem E) (computer programmer))) (supervisor (Tweakit Lem E) (Bitdiddle Ben)))

(and (supervisor ?x ?y)
     (not (job ?x (computer programmer))))

;; 4.56 b
(and (salary (Bitdiddle Ben) ?wizard-salary)
     (salary ?person ?amount)
     (lisp-value < ?amount ?wizard-salary))

(and (salary (Bitdiddle Ben) ?wizard-salary)
     (lisp-value < ?amount ?wizard-salary)
     (salary ?person ?amount))

(and (lisp-value < ?amount ?wizard-salary)
     (salary (Bitdiddle Ben) ?wizard-salary)
     (salary ?person ?amount))

;; 4.56 c
(and (supervisor ?x ?boss)
     (not (job ?boss (computer . ?position)))
     (job ?boss ?boss-position))

(and (not (job ?boss (computer . ?position)))
     (supervisor ?x ?boss)
     (job ?boss ?boss-position))

;; 4.57
(assert! (rule (same ?x ?x)))
(assert! (rule (replace ?person-1 ?person-2)
               (and (not (same ?person-1 ?person-2))
                    (job ?person-1 ?person-1-job)
                    (job ?person-2 ?person-2-job)
                    (or (same ?person-1-job ?person-2-job)
                        (can-do-job ?person-1-job ?person-2-job)))))
;; a:
(replace ?x (Fect Cy D))
;; b:
(and (lisp-value < ?a-salary ?b-salary)
     (replace ?a ?b)
     (salary ?a ?a-salary)
     (salary ?b ?b-salary))
