#lang racket/base

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

;; add to dispatch of make-new-machine
;; [(eq? message 'insts) the-instruction-sequence]
;; inside make-new-machine
(define (execute)
  (let ([insts (get-contents pc)])
    (if (null? insts)
        'done
        (let ([inst (car insts)])
          (when trace
            (when (not (null? (instruction-label inst)))
              (displayln (list 'Label: (instruction-label inst))))
            (displayln (list 'Executing: (instruction-text inst))))
          (when (breakpoint? inst) ;; ***
            (let br-loop ([input (read)])
              (when (not (tagged-list? input 'proceed-machine))
                (displayln (eval input ns))
                (br-loop (read)))))
          ((instruction-execution-proc inst))
          (set! inst-counts (add1 inst-counts))
          (execute)))))

(define (get-insts machine)
  (machine 'insts))

(define (make-instruction text) (mcons text (mcons null (mcons #f null)))) ;; ***
(define (instruction-text inst) (mcar inst))
(define (instruction-execution-proc inst) (mcdr (mcdr (mcdr inst)))) ;; ***
(define (set-instruction-execution-proc! inst proc)
  (set-mcdr! (mcdr (mcdr inst)) proc))  ;; ***
(define (instruction-label inst)
  (mcar (mcdr inst)))
(define (set-instruction-lable! inst label)
  (set-mcar! (mcdr inst) label))
(define (breakpoint? inst)
  (mcar (mcdr (mcdr inst))))
(define (inst-set-br inst)
  (set-mcar! (mcdr (mcdr inst)) #t))
(define (inst-cancel-br inst)
  (set-mcar! (mcdr (mcdr inst)) #f))

(define (set-or-cancel-br machine label n set-br?)
  (define (iter insts found-label count)
    (cond [(null? insts) (error "Can't find instruction to add or cancel breakpoint")]
          [(and found-label (zero? count))
           (if set-br?
               (inst-set-br (car insts))
               (inst-cancel-br (car insts)))
           'done]
          [found-label
           (iter (cdr insts) found-label (sub1 count))]
          [else
           (let* ([inst (car insts)]
                  [inst-label (instruction-label inst)])
             (if (and (not (null? inst-label))
                      (equal? label inst-label))
                 (iter insts #t count)
                 (iter (cdr insts) found-label count)))]))
  (if (< n 1)
      (error "n should be at least one")
      (iter (get-insts machine)
            #f
            (sub1 n))))

(define (set-breakpoint machine label n)
  (set-or-cancel-br machine label n #t))

(define (cancel-breakpoint machine label n)
  (set-or-cancel-br machine label n #f))

(define (cancel-all-breakpoints machine)
  (for-each
   (lambda (inst)
     (inst-cancel-br inst))
   (get-insts machine))
  'done)

;; test
(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b (test (op =) (reg b) (const 0))
            (branch (label gcd-done))
            (assign t (op rem) (reg a) (reg b))
            (assign a (reg b))
            (assign b (reg t))
            (goto (label test-b))
            gcd-done)))
(set-register-contents! gcd-machine 'a 206)
(set-register-contents! gcd-machine 'b 40)
(trace-on gcd-machine)
(set-breakpoint gcd-machine 'test-b 4)
(start gcd-machine)
;; input any procedure you want
(get-register-contents gcd-machine 'a)
;; 206
(proceed-machine gcd-machine)
(get-register-contents gcd-machine 'a)
;; 2
