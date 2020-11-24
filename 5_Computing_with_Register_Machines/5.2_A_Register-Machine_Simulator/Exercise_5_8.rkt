#lang racket/base

;; `assoc`(in `lookup-label`) returns the first matching element,
;; therefore the first label is used, `a` is 3.

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
