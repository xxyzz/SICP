#lang racket/base

;; see exercise 3.8
;; compiled code evaluate subexpressions from right to left

;; from left to right:
(define (construct-arglist operand-codes) ;; don't revert operand list
  (if (null? operand-codes)
      (make-instruction-sequence '() '(argl)
                                 '((assign argl (const ()))))
      (let ([code-to-get-first-arg
             (append-instruction-sequences
              (car operand-codes)
              (make-instruction-sequence '(val) '(argl)
                                         '((assign argl (op list) (reg val)))))])
        (if (null? (cdr operand-codes))
            code-to-get-first-arg
            (preserving '(env)
                        code-to-get-first-arg
                        (code-to-get-rest-args
                         (cdr operand-codes)))))))
(define (code-to-get-rest-args operand-codes)
  (let ([code-for-next-arg
         (preserving '(argl)
                     (car operand-codes)
                     (make-instruction-sequence
                      '(val argl)
                      '(argl)
                      '((assign val (op list) (reg val)) ;; ***
                        (assign argl
                                (op append) (reg argl) (reg val)))))]) ;; ***
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
                    code-for-next-arg
                    (code-to-get-rest-args (cdr operand-codes))))))

;; now the compiler don't need to reverse argument list but will
;; add an assign command in the compiled code for each argument
