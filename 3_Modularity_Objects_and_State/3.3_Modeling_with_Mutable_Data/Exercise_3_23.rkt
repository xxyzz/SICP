#lang racket/base

(define (front-ptr deque) (mcar deque))
(define (rear-ptr deque) (mcdr deque))

(define (set-front-ptr! deque item)
  (set-mcar! deque item))

(define (set-rear-ptr! deque item)
  (set-mcdr! deque item))

(define (empty-deque? deque)
  (null? (front-ptr deque)))

(define (make-deque) (mcons '() '()))

(define (set-prev-ptr! pair new-pair)
  (when (and (not (null? pair)) (not (null? (mcdr pair))))
    (set-mcar! (mcdr pair) new-pair)))

(define (set-next-ptr! pair new-pair)
  (when (and (not (null? pair)) (not (null? (mcdr pair))))
    (set-mcdr! (mcdr pair) new-pair)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (mcar (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (mcar (rear-ptr deque))))

(define (rear-insert-deque! deque item)
  (let ([new-pair (mcons item (mcons '() '()))])
    (cond [(empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque]
          [else
           (set-next-ptr! (rear-ptr deque) new-pair)
           (set-prev-ptr! new-pair (rear-ptr deque))
           (set-rear-ptr! deque new-pair)
           deque])))

(define (front-insert-deque! deque item)
  (let ([new-pair (mcons item (mcons '() '()))])
    (cond [(empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque]
          [else
           (set-prev-ptr! (front-ptr deque) new-pair)
           (set-next-ptr! new-pair (front-ptr deque))
           (set-front-ptr! deque new-pair)
           deque])))

(define (front-delete-deque! deque)
  (cond [(empty-deque? deque)
         (error "DELETE! called with an empty deque" deque)]
        [else (set-front-ptr! deque (mcdr (mcdr (front-ptr deque))))
              (set-prev-ptr! (front-ptr deque) null)
              deque]))

(define (rear-delete-deque! deque)
  (cond [(empty-deque? deque)
         (error "DELETE! called with an empty deque" deque)]
        [(eq? (front-ptr deque) (rear-ptr deque))
         (set-front-ptr! deque null)
         (set-rear-ptr! deque null)
         deque]
        [else (set-rear-ptr! deque (mcar (mcdr (rear-ptr deque))))
              (set-next-ptr! (rear-ptr deque) null)
              deque]))

(define (next-pair item)
  (cond [(null? item) null]
        [else (mcdr (mcdr item))]))

(define (last-pair x)
  (if (null? (next-pair x))
      x
      (last-pair (next-pair x))))

(define (make-cycle deque)
  (set-next-ptr! (last-pair (front-ptr deque)) (front-ptr deque))
  deque)

(define (contain-cycle deque)
  (let* ([tortoise (front-ptr deque)]
         [hare (next-pair (next-pair tortoise))])
    (define (iter tortoise hare)
      (cond [(and (null? tortoise) (null? hare)) #f]
            [(eq? tortoise hare) #t]
            [else
             (set! tortoise (next-pair tortoise))
             (set! hare (next-pair (next-pair hare)))
             (iter tortoise hare)]))
    (iter tortoise hare)))

(define (print-deque deque)
  (define (iter item)
    (when (not (null? item))
      (when (not (eq? item (front-ptr deque)))
        (display " "))
      (display (mcar item))
      (iter (next-pair item))))
  (if (contain-cycle deque)
      (display "This deque contains cycle\n")
      (begin 
        (display "'(")
        (iter (front-ptr deque))
        (display ")\n"))))

(define q1 (make-deque))
(front-insert-deque! q1 'a)
; (mcons (mcons 'a '()) (mcons 'a '()))
(print-deque q1)
; '(a)

(rear-insert-deque! q1 'b)
; (mcons #0=(mcons 'a (mcons '() #1=(mcons 'b (mcons #0# '())))) #1#)
(print-deque q1)
; '(a b)

(front-delete-deque! q1)
; (mcons (mcons 'b (mcons '() '())) (mcons 'b (mcons '() '())))
(print-deque q1)
; '(b)

(rear-delete-deque! q1)
; (mcons '() '())
(print-deque q1)
; '()

(define z (make-deque))
(front-insert-deque! z 'a)
; (mcons (mcons 'a (mcons '() '())) (mcons 'a (mcons '() '())))
(rear-insert-deque! z 'b)
; (mcons #0=(mcons 'a (mcons '() #1=(mcons 'b (mcons #0# '())))) #1#)
(print-deque z)
; '(a b)
(print-deque (make-cycle z))
; This deque contains cycle
