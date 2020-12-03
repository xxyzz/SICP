#lang racket/base

(define (make-register name)
  (let ([contents '*unassigned*]
        [trace #f])
    (define (dispatch message)
      (cond [(eq? message 'get) contents]
            [(eq? message 'set)
             (lambda (value)
               (when trace ;; ***
                 (displayln (list 'Reg-name: name
                                  'old-content: contents
                                  'new-content: value)))
               (set! contents value))]
            [(eq? message 'trace-on) ;; ***
             (set! trace #t)]
            [(eq? message 'tarce-off) ;; ***
             (set! trace #f)]
            [else
             (error "Unknown request: REGISTER" message)]))
    dispatch))

(define (reg-trace-on machine reg-name)
  ((get-register machine reg-name) 'trace-on)
  'reg-trace-on)

(define (reg-trace-off machine reg-name)
  ((get-register machine reg-name) 'trace-off)
  'reg-trace-off)

;; test
;; same machine as exercise 5.14
(trace-on factorial-machine)
(reg-trace-on factorial-machine 'n)
(start factorial-machine)
