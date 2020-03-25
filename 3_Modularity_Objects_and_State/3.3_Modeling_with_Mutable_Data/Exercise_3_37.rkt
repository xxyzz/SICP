#lang racket/base

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond [(null? items) 'done]
          [(eq? (car items) exception) (loop (cdr items))]
          [else (procedure (car items))
                (loop (cdr items))]))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (make-connector)
  (let ([value #f] [informant #f] [constraints '()])
    (define (set-my-value newval setter)
      (cond [(not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints)]
            [(not (= value newval))
             (error "Contradiction" (list value newval))]
            [else 'ignored]))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant #f)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (when (not (memq new-constraint constraints))
        (set! constraints
              (cons new-constraint constraints)))
      (when (has-value? me)
        (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond [(eq? request 'has-value?)
             (if informant #t #f)]
            [(eq? request 'value) value]
            [(eq? request 'set-value!) set-my-value]
            [(eq? request 'forget) forget-my-value]
            [(eq? request 'connect) connect]
            [else (error "Unknown operation: CONNECTOR"
                         request)]))
    me))

(define (probe name connector)
  (define (print-probe value)
    (display "Probe: ") (display name)
    (display " = ") (display value) (newline))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value) (print-probe "?"))
  (define (me request)
    (cond [(eq? request 'I-have-a-value) (process-new-value)]
          [(eq? request 'I-lost-my-value) (process-forget-value)]
          [else (error "Unknown request: PROBE" request)]))
  (connect connector me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond [(or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me)]
          [(and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me)]
          [(and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product)
                          (get-value m1)) me)]
          [(and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me)]))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond [(eq? request 'I-have-a-value) (process-new-value)]
          [(eq? request 'I-lost-my-value) (process-forget-value)]
          [else (error "Unknown request: MULTIPLIER"
                       request)]))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond [(and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me)]
          [(and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me)]
          [(and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me)]))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond [(eq? request 'I-have-a-value) (process-new-value)]
          [(eq? request 'I-lost-my-value) (process-forget-value)]
          [else (error "Unknown request: ADDER" request)]))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (c+ x y)
  (let ([z (make-connector)])
    (adder x y z)
    z))

(define (c- x y)
  (let ([z (make-connector)])
    (adder y z x)
    z))

(define (c* x y)
  (let ([z (make-connector)])
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ([z (make-connector)])
    (multiplier y z x)
    z))

(define (cv value)
  (let ([z (make-connector)])
    (constant value z)
    z))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))
(probe "C" C)
; #<procedure:me>
(probe "F" F)
; #<procedure:me>
(set-value! C 5 'user)
; Probe: C = 5
; Probe: F = 41
; 'done
(forget-value! C 'user)
; Probe: C = ?
; Probe: F = ?
; 'done
(set-value! F 41 'user)
; Probe: F = 41
; Probe: C = 5
; 'done
