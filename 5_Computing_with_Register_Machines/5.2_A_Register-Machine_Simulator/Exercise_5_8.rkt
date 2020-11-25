#lang racket/base

;; `assoc`(in `lookup-label`) returns the first matching element
;; and latter labels are `cons` before former labels
;; therefore the second label is used, `a` is 4.
;; need to check it after reading this chapter

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
