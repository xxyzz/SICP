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
        (list '= =)
        (list '> >)
        (list '< <)))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
(define apply-in-underlying-scheme apply)
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

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

(define (mmap proc . args)
  (if (null? (car args))
      null
      (mcons
       (apply proc (map car args))
       (apply mmap
              (cons proc (map cdr args))))))

(define (make-frame variables values)
  (mmap mcons variables values))
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

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))
(define (user-print object)
  (cond [(compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>))]
        [(compiled-procedure? object)
         (display '<compiled-procedure>)]
        [else (display object)]))

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
        'not-found
        (scan (first-frame env) 0)))
  (env-loop exp compile-env 0))

(define (scan-out-defines body)
  (define (body-loop exp vars vals rest-exp)
    (cond [(null? exp)
           (if (null? vars)
               rest-exp
               (list
                (cons
                 (make-lambda vars
                              (append (map (lambda (x y)
                                             (list 'set! x y))
                                           vars
                                           vals)
                                      rest-exp))
                 (map (lambda (x) '(quote *unassigned*)) vars))))]
          [(definition? (car exp))
           (body-loop (cdr exp)
                      (cons (cadar exp) vars)
                      (cons (caddar exp) vals)
                      rest-exp)]
          [else (body-loop (cdr exp)
                           vars
                           vals
                           (if (null? rest-exp)
                               (list (car exp))
                               (append rest-exp (list (car exp)))))]))
  (body-loop body null null null))

(define eceval-operations
  (list (list 'prompt-for-input prompt-for-input) ;; read-eval-print-loop
        (list 'read read)
        (list 'get-global-environment get-global-environment)
        (list 'announce-output announce-output)
        (list 'user-print user-print)
        ;; eval-dispatch
        (list 'self-evaluating? self-evaluating?)
        (list 'variable? variable?)
        (list 'quoted? quoted?)
        (list 'assignment? assignment?)
        (list 'definition? definition?)
        (list 'if? if?)
        (list 'lambda? lambda?)
        (list 'begin? begin?)
        (list 'application? application?)
        ;; ev-variable
        (list 'lookup-variable-value lookup-variable-value)
        ;; ev-quoted
        (list 'text-of-quotation text-of-quotation)
        ;; ev-lambda
        (list 'lambda-parameters lambda-parameters)
        (list 'lambda-body lambda-body)
        (list 'make-procedure make-procedure)
        ;; ev-application
        (list 'operands operands)
        (list 'operator operator)
        ;; ev-appl-did-operator
        (list 'empty-arglist empty-arglist)
        (list 'no-operands? no-operands?)
        ;; ev-appl-operand-loop
        (list 'first-operand first-operand)
        (list 'last-operand? last-operand?)
        ;; ev-appl-accumulate-arg
        (list 'adjoin-arg adjoin-arg)
        (list 'rest-operands rest-operands)
        ;; apply-dispatch
        (list 'primitive-procedure? primitive-procedure?)
        (list 'compound-procedure? compound-procedure?)
        (list 'compiled-procedure? compiled-procedure?)
        ;; primitive-apply
        (list 'apply-primitive-procedure apply-primitive-procedure)
        ;; compound-apply
        (list 'procedure-parameters procedure-parameters)
        (list 'procedure-environment procedure-environment)
        (list 'extend-environment extend-environment)
        (list 'procedure-body procedure-body)
        ;; ev-begin
        (list 'begin-actions begin-actions)
        ;; ev-sequence
        (list 'first-exp first-exp)
        (list 'last-exp? last-exp?)
        ;; ev-sequence-continue
        (list 'rest-exps rest-exps)
        ;; ev-if
        (list 'if-predicate if-predicate)
        ;; ev-if-decide
        (list 'true? true?)
        ;; ev-if-alternative
        (list 'if-alternative if-alternative)
        ;; ev-if-consequent
        (list 'if-consequent if-consequent)
        ;; ev-assignment
        (list 'assignment-variable assignment-variable)
        (list 'assignment-value assignment-value)
        ;; ev-assignment-1
        (list 'set-variable-value! set-variable-value!)
        ;; ev-definition
        (list 'definition-variable definition-variable)
        (list 'definition-value definition-value)
        ;; ev-definition-1
        (list 'define-variable! define-variable!)
        ;; compiled-apply
        (list 'compiled-procedure-entry compiled-procedure-entry)
        ;; compiler
        (list 'make-compiled-procedure make-compiled-procedure)
        (list 'compiled-procedure-env compiled-procedure-env)
        (list 'lexical-address-lookup lexical-address-lookup)
        (list 'false? false?)
        (list 'list list)
        ;; open-code
        (list '= =)
        (list '* *)
        (list '- -)))

(define eceval
  (make-machine
   '(exp env val proc argl continue unev arg1 arg2)
   eceval-operations
   '((branch (label external-entry)) ; branches if flag is set
     read-eval-print-loop
     (perform (op initialize-stack))
     (perform
      (op prompt-for-input) (const ";;EC-Eval input:"))
     (assign exp (op read))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (label eval-dispatch))
     print-result
     (perform (op print-stack-statistics)) ;; added instruction
     (perform (op announce-output) (const ";;EC-Eval value:"))
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))

     unknown-expression-type
     (assign val (const unknown-expression-type-error))
     (goto (label signal-error))
     unknown-procedure-type
     (restore continue) ;; clean up stack (from apply-dispatch)
     (assign val (const unknown-procedure-type-error))
     (goto (label signal-error))
     signal-error
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))

     eval-dispatch
     (test (op self-evaluating?) (reg exp))
     (branch (label ev-self-eval))
     (test (op variable?) (reg exp))
     (branch (label ev-variable))
     (test (op quoted?) (reg exp))
     (branch (label ev-quoted))
     (test (op assignment?) (reg exp))
     (branch (label ev-assignment))
     (test (op definition?) (reg exp))
     (branch (label ev-definition))
     (test (op if?) (reg exp))
     (branch (label ev-if))
     (test (op lambda?) (reg exp))
     (branch (label ev-lambda))
     (test (op begin?) (reg exp))
     (branch (label ev-begin))
     (test (op application?) (reg exp))
     (branch (label ev-application))
     (goto (label unknown-expression-type))

     ev-self-eval
     (assign val (reg exp))
     (goto (reg continue))
     ev-variable
     (assign val (op lookup-variable-value) (reg exp) (reg env))
     (goto (reg continue))
     ev-quoted
     (assign val (op text-of-quotation) (reg exp))
     (goto (reg continue))
     ev-lambda
     (assign unev (op lambda-parameters) (reg exp))
     (assign exp (op lambda-body) (reg exp))
     (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
     (goto (reg continue))

     ev-application
     (save continue)
     (save env)
     (assign unev (op operands) (reg exp))
     (save unev)
     (assign exp (op operator) (reg exp))
     (assign continue (label ev-appl-did-operator))
     (goto (label eval-dispatch))

     ev-appl-did-operator
     (restore unev) ;; the operands
     (restore env)
     (assign argl (op empty-arglist))
     (assign proc (reg val)) ;; the operator
     (test (op no-operands?) (reg unev))
     (branch (label apply-dispatch))
     (save proc)

     ev-appl-operand-loop
     (save argl)
     (assign exp (op first-operand) (reg unev))
     (test (op last-operand?) (reg unev))
     (branch (label ev-appl-last-arg))
     (save env)
     (save unev)
     (assign continue (label ev-appl-accumulate-arg))
     (goto (label eval-dispatch))

     ev-appl-accumulate-arg
     (restore unev)
     (restore env)
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (assign unev (op rest-operands) (reg unev))
     (goto (label ev-appl-operand-loop))

     ev-appl-last-arg
     (assign continue (label ev-appl-accum-last-arg))
     (goto (label eval-dispatch))
     ev-appl-accum-last-arg
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (restore proc)
     (goto (label apply-dispatch))

     apply-dispatch
     (test (op primitive-procedure?) (reg proc))
     (branch (label primitive-apply))
     (test (op compound-procedure?) (reg proc))
     (branch (label compound-apply))
     (test (op compiled-procedure?) (reg proc))
     (branch (label compiled-apply))
     (goto (label unknown-procedure-type))

     compiled-apply
     (restore continue)
     (assign val (op compiled-procedure-entry) (reg proc))
     (goto (reg val))

     primitive-apply
     (assign val (op apply-primitive-procedure)
             (reg proc)
             (reg argl))
     (restore continue)
     (goto (reg continue))

     compound-apply
     (assign unev (op procedure-parameters) (reg proc))
     (assign env (op procedure-environment) (reg proc))
     (assign env (op extend-environment)
             (reg unev) (reg argl) (reg env))
     (assign unev (op procedure-body) (reg proc))
     (goto (label ev-sequence))

     ev-begin
     (assign unev (op begin-actions) (reg exp))
     (save continue)
     (goto (label ev-sequence))

     ev-sequence
     (assign exp (op first-exp) (reg unev))
     (test (op last-exp?) (reg unev))
     (branch (label ev-sequence-last-exp))
     (save unev)
     (save env)
     (assign continue (label ev-sequence-continue))
     (goto (label eval-dispatch))
     ev-sequence-continue
     (restore env)
     (restore unev)
     (assign unev (op rest-exps) (reg unev))
     (goto (label ev-sequence))
     ev-sequence-last-exp
     (restore continue)
     (goto (label eval-dispatch))

     ev-if
     (save exp) ;; save expression for later
     (save env)
     (save continue)
     (assign continue (label ev-if-decide))
     (assign exp (op if-predicate) (reg exp))
     (goto (label eval-dispatch)) ;; evaluate the predicate

     ev-if-decide
     (restore continue)
     (restore env)
     (restore exp)
     (test (op true?) (reg val))
     (branch (label ev-if-consequent))
     ev-if-alternative
     (assign exp (op if-alternative) (reg exp))
     (goto (label eval-dispatch))
     ev-if-consequent
     (assign exp (op if-consequent) (reg exp))
     (goto (label eval-dispatch))

     ev-assignment
     (assign unev (op assignment-variable) (reg exp))
     (save unev) ;; save variable for later
     (assign exp (op assignment-value) (reg exp)) (save env)
     (save continue)
     (assign continue (label ev-assignment-1))
     (goto (label eval-dispatch)) ;; evaluate the assignment value
     ev-assignment-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform
      (op set-variable-value!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))

     ev-definition
     (assign unev (op definition-variable) (reg exp))
     (save unev) ;; save variable for later
     (assign exp (op definition-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-definition-1))
     (goto (label eval-dispatch)) ;; evaluate the definition value
     ev-definition-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform
      (op define-variable!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))

     external-entry
     (perform (op initialize-stack))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (reg val)))))

(define (start-eceval)
  (set! the-global-environment (setup-environment))
  (set-register-contents! eceval 'flag #f)
  (start eceval))

(define (compile-and-go expression)
  (let ([instructions
         (assemble
          (statements
           (compile expression 'val 'return the-empty-environment))
          eceval)])
    (set! the-global-environment (setup-environment))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag #t)
    (start eceval)))

(compile-and-go
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))

;;EC-Eval input:
;; (factorial 1)

;; (total-pushes = 5 maximum-depth = 3)

;;EC-Eval value:
1

;;EC-Eval input:
;; (factorial 2)

;; (total-pushes = 7 maximum-depth = 3)

;;EC-Eval value:
2

;;EC-Eval input:
;; (factorial 3)

;; (total-pushes = 9 maximum-depth = 4)

;;EC-Eval value:
6

;;EC-Eval input:
;; (factorial 4)

;; (total-pushes = 11 maximum-depth = 6)

;;EC-Eval value:
24

;;EC-Eval input:
;; (factorial 5)

;; (total-pushes = 13 maximum-depth = 8)

;;EC-Eval value:
120

;; compiled factorial
;; total-pushes: 2n + 3
;; maximum-depth: 2(n - 1)

;; special-purpose factorial
;; total-pushes: 2(n - 1)
;; maximum-depth: 2(n - 1)

;; interpreted recursive factorial
;; total-pushes: 32n - 16
;; maximum-depth: 5n + 3

;; compiled/interpreted
;; total-pushes: 2/32
;; maximum-depth: 2/5

;; special-purpose/interpreted
;; total-pushes: 2/32
;; maximum-depth: 2/5

(statements
 (compile
  '(define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n)))
  'val
  'next
  the-empty-environment))

'((assign val (op make-compiled-procedure) (label entry1) (reg env))
  (goto (label after-lambda2))
  entry1
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (assign arg1 (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign arg2 (const 1))
  (assign val (op =) (reg arg1) (reg arg2))
  (test (op false?) (reg val))
  (branch (label false-branch4))
  true-branch3
  (assign val (const 1))
  (goto (reg continue))
  false-branch4
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const factorial) (reg env))
  (assign arg1 (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign arg2 (const 1))
  (assign val (op -) (reg arg1) (reg arg2))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch6))
  compiled-branch7
  (assign continue (label proc-return9))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  proc-return9
  (assign arg1 (reg val))
  (goto (label after-call8))
  primitive-branch6
  (assign arg1 (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call8
  (restore env)
  (assign arg2 (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign val (op *) (reg arg1) (reg arg2))
  (restore continue)
  (goto (reg continue))
  after-if5
  after-lambda2
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok)))
