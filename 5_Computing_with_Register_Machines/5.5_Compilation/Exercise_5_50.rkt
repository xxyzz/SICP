#lang racket/base
(require compatibility/mlist)

;; =====================================================
;; Metacircular evaluator
;; =====================================================
(define (true? x) (not (eq? x #f)))
(define (false? x) (eq? x #f))

(define (self-evaluating? exp)
  (cond [(number? exp) #t]
        [(string? exp) #t]
        [(null? exp) #t]
        [(boolean? exp) #t]
        [else #f]))

(define (variable? exp) (symbol? exp))
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (cond [(pair? exp) (eq? (car exp) tag)]
        [(mpair? exp) (eq? (mcar exp) tag)]
        [else #f]))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ;; formal parameters
                   (cddr exp)))) ;; body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond [(null? seq) seq]
        [(last-exp? seq) (first-exp seq)]
        [else (make-begin seq)]))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false ;; no else clause
      (let ([first (car clauses)]
            [rest (cdr clauses)])
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (let? exp) (tagged-list? exp 'let))
(define (let->combination exp)
  (let* ([bindings (cadr exp)]
         [var-list (map car bindings)]
         [exp-list (map cadr bindings)]
         [body (cddr exp)])
    (cons (make-lambda var-list body) exp-list)))
(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define apply-in-underlying-scheme apply)
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'caddr caddr)
        (list 'cadddr cadddr)
        (list 'caadr caadr)
        (list 'cdadr cdadr)
        (list 'cddr cddr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '> >)
        (list '< <)
        (list 'apply apply-primitive-procedure) ;; *** remove another 'primitive
        (list 'cadr cadr)
        (list 'length length)
        (list 'mcons mcons)
        (list 'map map)
        (list 'eq? eq?)
        (list 'mpair? mpair?)
        (list 'mcar mcar)
        (list 'mcdr mcdr)
        (list 'list->mlist list->mlist)
        (list 'mappend! mappend!)
        (list 'mlist mlist)
        (list 'number? number?)
        (list 'string? string?)
        (list 'symbol? symbol?)
        (list 'pair? pair?)
        (list 'error error)
        (list 'newline newline)
        (list 'display display)
        (list 'read read)))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (list->mlist (map mcons variables values))) ;; ***
(define (add-binding-to-frame! var val frame)
  (mappend! frame (mlist (mcons var val))))
(define (frame-unit-variable unit) (mcar unit))
(define (frame-unit-value unit) (mcdr unit))

(define (travsersing-env end-frame-proc find-proc end-env-proc env var)
  (define (env-loop env)
    (define (scan pairs)
      (let ([current-pair
             (if (mpair? pairs)
                 (mcar pairs)
                 null)])
        (cond [(null? current-pair)
               (end-frame-proc env)]
              [(eq? var (frame-unit-variable current-pair))
               (find-proc current-pair)]
              [else (scan (mcdr pairs))])))
    (if (eq? env the-empty-environment)
        (end-env-proc var)
        (scan (first-frame env))))
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

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (setup-environment)
  (let ([initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)])
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    (define-variable! 'null null initial-env) ;; ***
    initial-env))
(define the-global-environment (setup-environment))

;; =====================================================
;; Register-Machine simulator
;; =====================================================
(define (make-machine register-names ops controller-text)
  (let ([machine (make-new-machine)])
    (for-each
     (lambda (register-name)
       ((machine 'allocate-register) register-name))
     register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-register name)
  (let ([contents '*unassigned*])
    (define (dispatch message)
      (cond [(eq? message 'get) contents]
            [(eq? message 'set)
             (lambda (value) (set! contents value))]
            [else
             (error "Unknown request: REGISTER" message)]))
    dispatch))

(define (get-contents register) (register 'get))
(define (set-contents! register value)
  ((register 'set) value))

(define (make-stack)
  (let ([s '()]
        [number-pushes 0]
        [max-depth 0]
        [current-depth 0])
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ([top (car s)])
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (displayln (list 'total-pushes  '= number-pushes
                       'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond [(eq? message 'push) push]
            [(eq? message 'pop) (pop)]
            [(eq? message 'initialize) (initialize)]
            [(eq? message 'print-statistics)
             (print-statistics)]
            [else (error "Unknown request: STACK" message)]))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

(define (make-new-machine)
  (let ([pc (make-register 'pc)]
        [flag (make-register 'flag)]
        [stack (make-stack)]
        [the-instruction-sequence '()])
    (let ([the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics))))]
          [register-table
           (list (list 'pc pc) (list 'flag flag))])
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ([val (assoc name register-table)])
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ([insts (get-contents pc)])
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond [(eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute)]
              [(eq? message 'install-instruction-sequence)
               (lambda (seq)
                 (set! the-instruction-sequence seq))]
              [(eq? message 'allocate-register)
               allocate-register]
              [(eq? message 'get-register)
               lookup-register]
              [(eq? message 'install-operations)
               (lambda (ops)
                 (set! the-ops (append the-ops ops)))]
              [(eq? message 'stack) stack]
              [(eq? message 'operations) the-ops]
              [else (error "Unknown request: MACHINE"
                           message)]))
      dispatch)))

(define (start machine) (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)
(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (assemble controller-text machine)
  (extract-labels
   controller-text
   (lambda (insts labels)
     (update-insts! insts labels machine)
     insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ([next-inst (car text)])
           (if (symbol? next-inst)
               (begin
                 (duplicate-label? next-inst labels)
                 (receive insts
                     (cons (make-label-entry next-inst
                                             insts)
                           labels)))
               (receive (cons (make-instruction next-inst)
                              insts)
                   labels)))))))

(define (duplicate-label? label-name labels)
  (when (assoc label-name labels)
    (error "Duplicated label: ASSEMBLE"
           label-name)))

(define (update-insts! insts labels machine)
  (let ([pc (get-register machine 'pc)]
        [flag (get-register machine 'flag)]
        [stack (machine 'stack)]
        [ops (machine 'operations)])
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst)
         labels machine pc flag stack ops)))
     insts)))

(define (make-instruction text) (mcons text null))
(define (instruction-text inst) (mcar inst))
(define (instruction-execution-proc inst) (mcdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-mcdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ([val (assoc label-name labels)])
    (if val
        (cdr val)
        (error "Undefined label: ASSEMBLE"
               label-name))))

(define (make-execution-procedure
         inst labels machine pc flag stack ops)
  (cond [(eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc)]
        [(eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc)]
        [(eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc)]
        [(eq? (car inst) 'goto)
         (make-goto inst machine labels pc)]
        [(eq? (car inst) 'save)
         (make-save inst machine stack pc)]
        [(eq? (car inst) 'restore) (make-restore inst machine stack pc)]
        [(eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc)]
        [else
         (error "Unknown instruction type: ASSEMBLE"
                inst)]))

(define (make-assign inst machine labels operations pc)
  (let ([target
         (get-register machine (assign-reg-name inst))]
        [value-exp (assign-value-exp inst)])
    (let ([value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))])
      (lambda () ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
  (let ([condition (test-condition inst)])
    (if (operation-exp? condition)
        (let ([condition-proc
               (make-operation-exp
                condition machine labels operations)])
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction: ASSEMBLE" inst))))
(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ([dest (branch-dest inst)])
    (if (label-exp? dest)
        (let ([insts
               (lookup-label
                labels
                (label-exp-label dest))])
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction: ASSEMBLE" inst))))
(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ([dest (goto-dest inst)])
    (cond [(label-exp? dest)
           (let ([insts (lookup-label
                         labels
                         (label-exp-label dest))])
             (lambda () (set-contents! pc insts)))]
          [(register-exp? dest)
           (let ([reg (get-register
                       machine
                       (register-exp-reg dest))])
             (lambda ()
               (set-contents! pc (get-contents reg))))]
          [else (error "Bad GOTO instruction: ASSEMBLE" inst)])))
(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ([reg (get-register machine
                           (stack-inst-reg-name inst))])
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
(define (make-restore inst machine stack pc)
  (let ([reg (get-register machine
                           (stack-inst-reg-name inst))])
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))
(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ([action (perform-action inst)])
    (if (operation-exp? action)
        (let ([action-proc
               (make-operation-exp
                action machine labels operations)])
          (lambda () (action-proc) (advance-pc pc)))
        (error "Bad PERFORM instruction: ASSEMBLE" inst))))
(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond [(constant-exp? exp)
         (let ([c (constant-exp-value exp)])
           (lambda () c))]
        [(label-exp? exp)
         (let ([insts (lookup-label
                       labels
                       (label-exp-label exp))])
           (lambda () insts))]
        [(register-exp? exp)
         (let ([r (get-register machine (register-exp-reg exp))])
           (lambda () (get-contents r)))]
        [else (error "Unknown expression type: ASSEMBLE" exp)]))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ([op (lookup-prim (operation-exp-op exp)
                         operations)]
        [aprocs
         (map (lambda (e)
                (make-primitive-exp e machine labels))
              (operation-exp-operands exp))])
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ([val (assoc symbol operations)])
    (if val
        (cadr val)
        (error "Unknown operation: ASSEMBLE"
               symbol))))

(define (empty-arglist) '())
(define (adjoin-arg arg arglist) (append arglist (list arg)))
(define (last-operand? ops) (null? (cdr ops)))
(define (get-global-environment) the-global-environment)

;; =====================================================
;; Compiler
;; =====================================================
(define (compile exp target linkage compile-env)
  (cond [(self-evaluating? exp)
         (compile-self-evaluating exp target linkage)]
        [(quoted? exp) (compile-quoted exp target linkage)]
        [(variable? exp)
         (compile-variable exp target linkage compile-env)]
        [(assignment? exp)
         (compile-assignment exp target linkage compile-env)]
        [(definition? exp)
         (compile-definition exp target linkage compile-env)]
        [(if? exp) (compile-if exp target linkage compile-env)]
        [(lambda? exp) (compile-lambda exp target linkage compile-env)]
        [(begin? exp)
         (compile-sequence
          (begin-actions exp) target linkage compile-env)]
        [(cond? exp)
         (compile (cond->if exp) target linkage compile-env)]
        [(let? exp)
         (compile (let->combination exp) target linkage compile-env)]
        [(open-code? exp compile-env)
         (compile-open-code exp target linkage compile-env)]
        [(application? exp)
         (compile-application exp target linkage compile-env)]
        [else
         (error "Unknown expression type: COMPILE" exp)]))

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

(define (compile-linkage linkage)
  (cond [(eq? linkage 'return)
         (make-instruction-sequence '(continue) '()
                                    '((goto (reg continue))))]
        [(eq? linkage 'next)
         (empty-instruction-sequence)]
        [else
         (make-instruction-sequence '() '()
                                    `((goto (label ,linkage))))]))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)))

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '()
    (list target)
    `((assign ,target (const ,exp))))))
(define (compile-quoted exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '()
    (list target)
    `((assign ,target (const ,(text-of-quotation exp)))))))
(define (compile-variable exp target linkage compile-env)
  (let ([address (find-variable exp compile-env)])
    (end-with-linkage
     linkage
     (if (eq? address 'not-found)
         (make-instruction-sequence
          '(env)
          (list target)
          `((assign ,target (op lookup-variable-value) (const ,exp) (reg env))))
         (make-instruction-sequence
          '(env)
          (list target)
          `((assign ,target
                    (op lexical-address-lookup)
                    (const ,address)
                    (reg env))))))))

(define (compile-assignment exp target linkage compile-env)
  (let* ([var (assignment-variable exp)]
         [get-value-code
          (compile (assignment-value exp) 'val 'next compile-env)]
         [address (find-variable var compile-env)])
    (end-with-linkage
     linkage
     (preserving
      '(env)
      get-value-code
      (if (eq? address 'not-found)
          (make-instruction-sequence
           '(env val)
           (list target 'env)
           `((assign env (op get-global-environment))
             (perform (op set-variable-value!)
                      (const ,var)
                      (reg val)
                      (reg env))
             (assign ,target (const ok))))
          (make-instruction-sequence
           '(env val)
           (list target)
           `((perform (op lexical-address-set!)
                      (const ,address)
                      (reg val)
                      (reg env))
             (assign ,target (const ok)))))))))
(define (compile-definition exp target linkage compile-env)
  (let ([var (definition-variable exp)]
        [get-value-code
         (compile (definition-value exp) 'val 'next compile-env)])
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

(define label-counter 0)
(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)
(define (make-label name)
  (string->symbol
   (string-append (symbol->string name)
                  (number->string (new-label-number)))))

(define (compile-if exp target linkage compile-env)
  (let ([t-branch (make-label 'true-branch)]
        [f-branch (make-label 'false-branch)]
        [after-if (make-label 'after-if)])
    (let ([consequent-linkage
           (if (eq? linkage 'next) after-if linkage)])
      (let ([p-code (compile (if-predicate exp) 'val 'next compile-env)]
            [c-code
             (compile
              (if-consequent exp) target consequent-linkage compile-env)]
            [a-code
             (compile (if-alternative exp) target linkage compile-env)])
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
      (compile (first-exp seq) target linkage compile-env)
      (preserving '(env continue)
                  (compile (first-exp seq) target 'next compile-env)
                  (compile-sequence
                   (rest-exps seq)
                   target
                   linkage
                   compile-env))))

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

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
        (compile-lambda-body exp proc-entry compile-env))
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
     (compile-sequence (scan-out-defines (lambda-body exp))
                       'val
                       'return
                       (cons formals compile-env)))))

(define (compile-application exp target linkage compile-env)
  (let ([proc-code (compile (operator exp) 'proc 'next compile-env)]
        [operand-codes
         (map (lambda (operand) (compile operand 'val 'next compile-env))
              (operands exp))])
    (preserving '(env continue)
                proc-code
                (preserving '(proc continue)
                            (construct-arglist operand-codes)
                            (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ([operand-codes (reverse operand-codes)])
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
                                   '((assign argl (const ()))))
        (let ([code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                                           '((assign argl (op list) (reg val)))))])
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
                          code-to-get-last-arg
                          (code-to-get-rest-args
                           (cdr operand-codes))))))))
(define (code-to-get-rest-args operand-codes)
  (let ([code-for-next-arg
         (preserving '(argl)
                     (car operand-codes)
                     (make-instruction-sequence
                      '(val argl)
                      '(argl)
                      '((assign argl
                                (op cons) (reg val) (reg argl)))))])
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
                    code-for-next-arg
                    (code-to-get-rest-args (cdr operand-codes))))))

(define (compile-procedure-call target linkage)
  (let ([primitive-branch (make-label 'primitive-branch)]
        [compiled-branch (make-label 'compiled-branch)]
        [after-call (make-label 'after-call)])
    (let ([compiled-linkage
           (if (eq? linkage 'next) after-call linkage)])
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
                                  `((test (op primitive-procedure?) (reg proc))
                                    (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage
          linkage
          (make-instruction-sequence '(proc argl)
                                     (list target)
                                     `((assign ,target
                                               (op apply-primitive-procedure)
                                               (reg proc)
                                               (reg argl)))))))
       after-call))))

(define (compile-proc-appl target linkage)
  (cond [(and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
                                    `((assign continue (label ,linkage))
                                      (assign val (op compiled-procedure-entry)
                                              (reg proc))
                                      (goto (reg val))))]
        [(and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ([proc-return (make-label 'proc-return)])
           (make-instruction-sequence '(proc) all-regs
                                      `((assign continue (label ,proc-return))
                                        (assign val (op compiled-procedure-entry)
                                                (reg proc))
                                        (goto (reg val))
                                        ,proc-return
                                        (assign ,target (reg val))
                                        (goto (label ,linkage)))))]
        [(and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
                                    '((assign val (op compiled-procedure-entry)
                                              (reg proc))
                                      (goto (reg val))))]
        [(and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target)]))

(define all-regs '(env proc val argl continue arg1 arg2))

(define (registers-needed s)
  (if (symbol? s) '() (car s)))
(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))
(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond [(null? s1) s2]
        [(memq (car s1) s2) (list-union (cdr s1) s2)]
        [else (cons (car s1) (list-union (cdr s1) s2))]))
(define (list-difference s1 s2)
  (cond [(null? s1) '()]
        [(memq (car s1) s2) (list-difference (cdr s1) s2)]
        [else (cons (car s1)
                    (list-difference (cdr s1) s2))]))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ([first-reg (car regs)])
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
                        (make-instruction-sequence
                         (list-union (list first-reg)
                                     (registers-needed seq1))
                         (list-difference (registers-modified seq1)
                                          (list first-reg))
                         (append `((save ,first-reg))
                                 (statements seq1)
                                 `((restore ,first-reg))))
                        seq2)
            (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1) (statements seq2))))

;; =========================================
;; optimizations from previous exercises
;; =========================================
(define (spread-arguments operand-list compile-env)
  (cons (compile (car operand-list) 'arg1 'next compile-env)
        (map (lambda (operand)
               (compile operand 'arg2 'next compile-env))
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
  (let ([operand-codes (spread-arguments (operands exp) compile-env)])
    (end-with-linkage
     linkage
     (preserving
      '(env)
      (car operand-codes)
      (compile-rest-open-codes (cdr operand-codes))))))

(define (open-code? exp compile-env)
  (let* ([var (operator exp)]
         [address (find-variable var compile-env)])
    (if (eq? address 'not-found)
        (memq var '(= * - +))
        #f)))

(define (make-lexical-address nth-frame nth-var)
  (list nth-frame nth-var))
(define (lexical-frame address) (car address))
(define (lexical-var address) (cadr address))

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

(define (find-variable exp compile-env)
  (define (env-loop env nth-frame)
    (define (scan vars nth-var)
      (let ([current-var (if (pair? vars) (car vars) null)])
        (cond [(null? current-var)
               (env-loop (enclosing-environment env) (add1 nth-frame))]
              [(eq? exp current-var)
               (make-lexical-address nth-frame nth-var)]
              [else
               (scan (cdr vars) (add1 nth-var))])))
    (if (eq? env the-empty-environment)
        'not-found
        (scan (first-frame env) 0)))
  (env-loop compile-env 0))

(define (scan-out-defines body)
  (define (body-loop exps vars vals rest-exps)
    (cond [(null? exps)
           (if (null? vars)
               rest-exps
               (list (make-let
                      (map (lambda (x) (list x ''*unassigned*)) vars)
                      (append (map (lambda (x y) (list 'set! x y)) vars vals)
                              rest-exps))))]
          [(definition? (car exps))
           (let* ([current-exp (car exps)]
                  [var (definition-variable current-exp)]
                  [val-list (definition-value current-exp)])
             (body-loop (cdr exps)
                        (if (null? vars)
                            (list var)
                            (append vars (list var)))
                        (if (null? vals)
                            (list val-list)
                            (append vals (list val-list)))
                        rest-exps))]
          [else (body-loop (cdr exps)
                           vars
                           vals
                           (if (null? rest-exps)
                               (list (car exps))
                               (append rest-exps (list (car exps)))))]))
  (body-loop body null null null))

(define machine
  (make-machine
   all-regs
   (list (list 'make-compiled-procedure make-compiled-procedure)
         (list 'compiled-procedure-env compiled-procedure-env)
         (list 'extend-environment extend-environment)
         (list 'lookup-variable-value lookup-variable-value)
         (list 'lexical-address-lookup lexical-address-lookup)
         (list 'list list)
         (list 'primitive-procedure? primitive-procedure?)
         (list 'compiled-procedure-entry compiled-procedure-entry)
         (list 'apply-primitive-procedure apply-primitive-procedure)
         (list 'false? false?)
         (list 'cons cons)
         (list 'define-variable! define-variable!)
         (list 'lexical-address-set! lexical-address-set!)
         (list '= =))
   (statements
    (compile
     '(begin
        ;; see exercise 4.14 and primitive-procedure-objects
        (define (mymap1 proc list)
          (if (null? list)
              null
              (cons (proc (car list))
                    (mymap1 proc (cdr list)))))
        (define (mymap2 proc list1 list2) ;; make-frame
          (if (null? list1)
              null
              (cons
               (proc (car list1) (car list2))
               (mymap2 proc (cdr list1) (cdr list2)))))

        (define (true? x) (not (eq? x #f)))
        (define (false? x) (eq? x #f))

        (define (list-of-values exps env)
          (if (no-operands? exps)
              '()
              (cons (eval (first-operand exps) env)
                    (list-of-values (rest-operands exps) env))))

        (define (eval-if exp env)
          (if (true? (eval (if-predicate exp) env))
              (eval (if-consequent exp) env)
              (eval (if-alternative exp) env)))

        (define (eval-sequence exps env)
          (cond [(last-exp? exps)
                 (eval (first-exp exps) env)]
                [else
                 (eval (first-exp exps) env)
                 (eval-sequence (rest-exps exps) env)]))

        (define (eval-assignment exp env)
          (set-variable-value! (assignment-variable exp)
                               (eval (assignment-value exp) env)
                               env)
          'ok)

        (define (eval-definition exp env)
          (define-variable! (definition-variable exp)
            (eval (definition-value exp) env)
            env)
          'ok)

        (define (self-evaluating? exp)
          (cond [(number? exp) #t]
                [(string? exp) #t]
                [(null? exp) #t]
                [else #f]))

        (define (variable? exp) (symbol? exp))
        (define (quoted? exp) (tagged-list? exp 'quote))
        (define (text-of-quotation exp) (cadr exp))

        (define (tagged-list? exp tag)
          (if (pair? exp)
              (eq? (car exp) tag)
              #f))

        (define (assignment? exp) (tagged-list? exp 'set!))
        (define (assignment-variable exp) (cadr exp))
        (define (assignment-value exp) (caddr exp))

        (define (definition? exp)
          (tagged-list? exp 'define))
        (define (definition-variable exp)
          (if (symbol? (cadr exp))
              (cadr exp)
              (caadr exp)))
        (define (definition-value exp)
          (if (symbol? (cadr exp))
              (caddr exp)
              (make-lambda (cdadr exp)   ;; formal parameters
                           (cddr exp)))) ;; body
        (define (lambda? exp) (tagged-list? exp 'lambda))
        (define (lambda-parameters exp) (cadr exp))
        (define (lambda-body exp) (cddr exp))
        (define (make-lambda parameters body)
          (cons 'lambda (cons parameters body)))

        (define (if? exp) (tagged-list? exp 'if))
        (define (if-predicate exp) (cadr exp))
        (define (if-consequent exp) (caddr exp))
        (define (if-alternative exp)
          (if (not (null? (cdddr exp)))
              (cadddr exp)
              'false))
        (define (make-if predicate consequent alternative)
          (list 'if predicate consequent alternative))

        (define (begin? exp) (tagged-list? exp 'begin))
        (define (begin-actions exp) (cdr exp))

        (define (last-exp? seq) (null? (cdr seq)))
        (define (first-exp seq) (car seq))
        (define (rest-exps seq) (cdr seq))

        (define (sequence->exp seq)
          (cond [(null? seq) seq]
                [(last-exp? seq) (first-exp seq)]
                [else (make-begin seq)]))
        (define (make-begin seq) (cons 'begin seq))

        (define (application? exp) (pair? exp))
        (define (operator exp) (car exp))
        (define (operands exp) (cdr exp))
        (define (no-operands? ops) (null? ops))
        (define (first-operand ops) (car ops))
        (define (rest-operands ops) (cdr ops))

        (define (cond? exp) (tagged-list? exp 'cond))
        (define (cond-clauses exp) (cdr exp))
        (define (cond-else-clause? clause)
          (eq? (cond-predicate clause) 'else))
        (define (cond-predicate clause) (car clause))
        (define (cond-actions clause) (cdr clause))
        (define (cond->if exp) (expand-clauses (cond-clauses exp)))
        (define (expand-clauses clauses)
          (if (null? clauses)
              'false ;; no else clause
              (let ([first (car clauses)]
                    [rest (cdr clauses)])
                (if (cond-else-clause? first)
                    (if (null? rest)
                        (sequence->exp (cond-actions first))
                        (error "ELSE clause isn't last: COND->IF"
                               clauses))
                    (make-if (cond-predicate first)
                             (sequence->exp (cond-actions first))
                             (expand-clauses rest))))))

        (define (primitive-procedure? proc)
          (tagged-list? proc 'primitive))
        (define (primitive-implementation proc) (cadr proc))

        (define primitive-procedures
          (list (list 'car car)
                (list 'cdr cdr)
                (list 'cons cons)
                (list 'null? null?)
                (list 'list list)
                (list '+ +)
                (list '- -)
                (list '* *)
                (list '/ /)
                (list '= =)))
        (define (primitive-procedure-names)
          (mymap1 car primitive-procedures))
        (define (primitive-procedure-objects)
          (mymap1 (lambda (proc) (list 'primitive (cadr proc))) ;; ***
                  primitive-procedures))
        (define apply-in-underlying-scheme apply)
        (define (apply-primitive-procedure proc args)
          (apply-in-underlying-scheme
           (primitive-implementation proc) args))

        (define (eval exp env)
          (cond [(self-evaluating? exp) exp]
                [(variable? exp) (lookup-variable-value exp env)]
                [(quoted? exp) (text-of-quotation exp)]
                [(assignment? exp)
                 (eval-assignment exp env)]
                [(definition? exp)
                 (eval-definition exp env)]
                [(if? exp) (eval-if exp env)]
                [(lambda? exp) (make-procedure (lambda-parameters exp)
                                               (lambda-body exp)
                                               env)]
                [(begin? exp)
                 (eval-sequence (begin-actions exp) env)]
                [(cond? exp) (eval (cond->if exp) env)]
                [(application? exp)
                 (myapply (eval (operator exp) env)
                          (list-of-values (operands exp) env))]
                [else
                 (error "Unknown expression type: EVAL" exp)]))

        (define (myapply procedure arguments)
          (cond [(primitive-procedure? procedure)
                 (apply-primitive-procedure procedure arguments)]
                [(compound-procedure? procedure)
                 (eval-sequence
                  (procedure-body procedure)
                  (extend-environment
                   (procedure-parameters procedure)
                   arguments
                   (procedure-environment procedure)))]
                [else
                 (error
                  "Unknown procedure type: APPLY" procedure)]))

        (define (make-procedure parameters body env)
          (list 'procedure parameters body env))
        (define (compound-procedure? p)
          (tagged-list? p 'procedure))
        (define (procedure-parameters p) (cadr p))
        (define (procedure-body p) (caddr p))
        (define (procedure-environment p) (cadddr p))

        (define (enclosing-environment env) (cdr env))
        (define (first-frame env) (car env))
        (define the-empty-environment '())

        (define (make-frame variables values)
          (list->mlist (mymap2 mcons variables values))) ;; ***
        (define (add-binding-to-frame! var val frame)
          (mappend! frame (mlist (mcons var val))))
        (define (frame-unit-variable unit) (mcar unit))
        (define (frame-unit-value unit) (mcdr unit))

        (define (travsersing-env end-frame-proc find-proc end-env-proc env var)
          (define (env-loop env)
            (define (scan pairs)
              (let ([current-pair
                     (if (mpair? pairs)
                         (mcar pairs)
                         null)])
                (cond [(null? current-pair)
                       (end-frame-proc env)]
                      [(eq? var (frame-unit-variable current-pair))
                       (find-proc current-pair)]
                      [else (scan (mcdr pairs))])))
            (if (eq? env the-empty-environment)
                (end-env-proc var)
                (scan (first-frame env))))
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

        (define (extend-environment vars vals base-env)
          (if (= (length vars) (length vals))
              (cons (make-frame vars vals) base-env)
              (if (< (length vars) (length vals))
                  (error "Too many arguments supplied" vars vals)
                  (error "Too few arguments supplied" vars vals))))

        (define (setup-environment)
          (let ([initial-env
                 (extend-environment (primitive-procedure-names)
                                     (primitive-procedure-objects)
                                     the-empty-environment)])
            (define-variable! 'true #t initial-env)
            (define-variable! 'false #f initial-env)
            initial-env))
        (define the-global-environment (setup-environment))

        (define input-prompt ";;; M-Eval input:")
        (define output-prompt ";;; M-Eval value:")
        (define (driver-loop)
          (prompt-for-input input-prompt)
          (let ([input (read)])
            (let ([output (eval input the-global-environment)])
              (announce-output output-prompt)
              (user-print output)))
          (driver-loop))
        (define (prompt-for-input string)
          (newline) (newline) (display string) (newline))
        (define (announce-output string)
          (newline) (display string) (newline))
        (define (user-print object)
          (if (compound-procedure? object)
              (display (list 'compound-procedure
                             (procedure-parameters object)
                             (procedure-body object)
                             '<procedure-env>))
              (display object)))
        (driver-loop))
     'val
     'next
     the-empty-environment))))
(set-register-contents! machine 'env (get-global-environment))
(start machine)
