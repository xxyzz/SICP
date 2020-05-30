#lang racket/base

;; (do ([id init-expr step-expr-maybe] ...)
;;     (stop?-expr finish-expr ...)
;;   expr ...)
;; https://docs.racket-lang.org/reference/for.html?q=do#%28form._%28%28lib._racket%2Fprivate%2Fmore-scheme..rkt%29._do%29%29

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (make-define id expr)
  (cons 'define (cons id expr)))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (make-begin seq) (cons 'begin seq))

(define (do->combination exp)
  (let ([var-list (map car (cadr exp))]
        [init-list (map cadr (cadr exp))]
        [step-list (map caddr (cadr exp))]
        [stop?-expr (caaddr exp)]
        [finish-expr (cdaddr exp)]
        [expr (cdddr exp)])
    (list
     (make-lambda null
                  (list
                   (make-define
                    (cons 'do-procedure var-list)
                    (list
                     (make-if stop?-expr
                              (make-begin finish-expr)
                              (make-begin
                               (append expr
                                       (list (cons 'do-procedure
                                                   step-list)))))))
                   (cons 'do-procedure init-list))))))

(define (eval-do exp env)
  (eval (do->combination exp) env))

(do->combination '(do ((i 0 (add1 i)))
                      ((> i 5) 'done)
                    (println i)))
;; '((lambda () (define (do-procedure i) (if (> i 5) (begin 'done) (begin (println i) (do-procedure (add1 i))))) (do-procedure 0)))

((lambda ()
   (define (do-procedure i)
     (if (> i 5)
         (begin 'done)
         (begin (println i)
                (do-procedure (add1 i)))))
   (do-procedure 0)))
;; 0
;; 1
;; 2
;; 3
;; 4
;; 5
;; 'done

