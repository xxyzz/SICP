#lang racket/base

(define (make-new-machine)
  (let ([pc (make-register 'pc)]
        [flag (make-register 'flag)]
        [stacks null]
        [the-instruction-sequence '()]
        [inst-counts 0]
        [trace #f]) ;; ***
    (let ([the-ops
           (list (list 'initialize-stack
                       (lambda ()
                         (for-each (lambda (stack)
                                     (stack 'initialize))
                                   stacks)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics))))]
          [register-table
           (list (list 'pc pc) (list 'flag flag))])
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (begin
              (set! stacks
                    (cons (cons name (make-stack))
                          stacks))
              (set! register-table
                    (cons (list name (make-register name))
                          register-table))))
        'register-allocated)
      (define (lookup-register name)
        (let ([val (assoc name register-table)])
          (if val
              (cadr val)
              (begin ;; create new register if not exist
                (allocate-register name)
                (lookup-register name)))))
      (define (execute)
        (let ([insts (get-contents pc)])
          (if (null? insts)
              'done
              (begin
                (when trace
                  (displayln (list 'Executing: (instruction-text (car insts))))) ;; ***
                ((instruction-execution-proc (car insts)))
                (set! inst-counts (add1 inst-counts))
                (execute)))))
      (define (dispatch message)
        (cond [(eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute)]
              [(eq? message 'install-instruction-sequence)
               (lambda (seq)
                 (set! the-instruction-sequence seq))]
              [(eq? message 'get-register)
               lookup-register]
              [(eq? message 'install-operations)
               (lambda (ops)
                 (set! the-ops (append the-ops ops)))]
              [(eq? message 'stacks) stacks]
              [(eq? message 'operations) the-ops]
              [(eq? message 'print-inst-counts)
               (displayln (list 'inst-counts '= inst-counts))]
              [(eq? message 'reset-inst-counts)
               (set! inst-counts 0)]
              [(eq? message 'trace-on) ;; ***
               (set! trace #t)]
              [(eq? message 'trace-off) ;; ***
               (set! trace #f)]
              [else (error "Unknown request: MACHINE"
                           message)]))
      dispatch)))

(define (trace-on machine)
  (machine 'trace-on)
  'machine-trace-on)

(define (trace-off machine)
  (machine 'trace-off)
  'machine-trace-off)
