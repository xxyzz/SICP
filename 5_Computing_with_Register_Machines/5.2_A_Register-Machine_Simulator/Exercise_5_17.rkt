#lang racket/base

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
                 (when (not (null? insts)) ;; ***
                   (set-instruction-lable! (car insts) next-inst))
                 (receive insts
                     (cons (make-label-entry next-inst
                                             insts)
                           labels)))
               (receive (cons (make-instruction next-inst)
                              insts)
                   labels)))))))

(define (make-instruction text) (mcons text (mcons null null))) ;; ***
(define (instruction-text inst) (mcar inst))
(define (instruction-execution-proc inst) (mcdr (mcdr inst))) ;; ***
(define (set-instruction-execution-proc! inst proc)
  (set-mcdr! (mcdr inst) proc))  ;; ***
(define (instruction-label inst) ;; new procedure
  (mcar (mcdr inst)))
(define (set-instruction-lable! inst label) ;; new procedure
  (set-mcar! (mcdr inst) label))

;; inside make-new-machine:
(define (execute)
  (let ([insts (get-contents pc)])
    (if (null? insts)
        'done
        (let ([inst (car insts)]) ;; ***
          (when trace
            (when (not (null? (instruction-label inst)))
              (displayln (list 'Label: (instruction-label inst))))
            (displayln (list 'Executing: (instruction-text inst))))
          ((instruction-execution-proc inst))
          (set! inst-counts (add1 inst-counts))
          (execute)))))
