#lang racket/base

;; a
(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (letrec->let exp)
  (let ([bindings (cadr exp)]
        [body (cddr exp)])
    (make-let (map (lambda (binding) (list (car binding) '*unasigned*)) bindings)
              (append
               (map (lambda (binding) (list 'set! (car binding) (cadr binding))) bindings)
               body))))

;; test
(letrec->let '(letrec
                  ((even? (lambda (n)
                            (if (= n 0) #t (odd? (- n 1)))))
                   (odd? (lambda (n)
                           (if (= n 0) #f (even? (- n 1))))))
                (even? 3)))
;; '(let ((even? '*unasigned*) (odd? '*unasigned*))
;;    (set! even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
;;    (set! odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))
;;    (even? 3))

;; b
;; http://community.schemewiki.org/?sicp-ex-4.20
