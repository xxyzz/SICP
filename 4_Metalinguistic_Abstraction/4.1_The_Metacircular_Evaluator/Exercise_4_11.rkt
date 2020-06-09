#lang racket/base

(define (mmap proc . args)
  (if (null? (car args))
      null
      (mcons
       (apply proc (map mcar args))
       (apply mmap
              (cons proc (map mcdr args))))))
(define (mlist . args)
  (define (mlist-iter a l)
    (if (null? a)
        l
        (mlist-iter (cdr a) (mcons (car a) l))))
  (mlist-iter args null))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (mmap mcons variables values))
(define (frame-variables frame) (mmap mcar frame))
(define (frame-values frame) (mmap mcdr frame))
(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (mcons (mcons var val) (mcar frame))))
(define (frame-unit-variable unit) (mcar unit))
(define (frame-unit-value unit) (mcdr unit))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan pairs)
      (let ([current-pair (mcar pairs)])
      (cond [(null? current-pair)
             (env-loop (enclosing-environment env))]
            [(eq? var (frame-unit-variable current-pair)) (set-mcdr! current-pair val)]
            [else (scan (mcdr pairs))])))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ([frame (first-frame env)])
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ([frame (first-frame env)])
    (define (scan pairs)
      (let ([current-pair (mcar pairs)])
      (cond [(null? current-pair)
             (add-binding-to-frame! var val frame)]
            [(eq? var (frame-unit-variable current-pair)) (set-mcdr! current-pair val)]
            [else (scan (mcdr pairs))])))
    (scan (frame-variables frame) (frame-values frame))))

;; test
(define frame-a (make-frame (mlist 'a 'b 'c) (mlist 1 2 3)))
frame-a
;; (mcons (mcons 'c 3) (mcons (mcons 'b 2) (mcons (mcons 'a 1) '())))
(frame-variables frame-a)
;; (mcons 'c (mcons 'b (mcons 'a '())))
(frame-values frame-a)
;; (mcons 3 (mcons 2 (mcons 1 '())))
(add-binding-to-frame! 'd 4 frame-a)
frame-a
;; (mcons (mcons 'c 3) (mcons (mcons 'd 4) (mcons (mcons 'b 2) (mcons (mcons 'a 1) '()))))
