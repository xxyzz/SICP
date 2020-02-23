#lang racket/base

(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))

(define (set-front-ptr! queue item)
  (set-mcar! queue item))

(define (set-rear-ptr! queue item)
  (set-mcdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (mcons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (mcar (front-ptr queue))))

(define (insert-queue! queue item)
  (let ([new-pair (mcons item '())])
    (cond [(empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue]
          [else
           (set-mcdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue])))

(define (delete-queue! queue)
  (cond [(empty-queue? queue)
         (error "DELETE! called with an empty queue" queue)]
        [else (set-front-ptr! queue (mcdr (front-ptr queue)))
              queue]))

(define (print-queue queue)
  (define (iter item)
    (when (not (null? item))
      (when (not (eq? (mcar item) (front-queue queue)))
        (display " "))
      (display (mcar item))
      (iter (mcdr item))))
  (display "'(")
  (iter (front-ptr queue))
  (display ")\n"))

(define q1 (make-queue))
(insert-queue! q1 'a)
; (mcons (mcons 'a '()) (mcons 'a '()))
(print-queue q1)
; '(a)

(insert-queue! q1 'b)
; (mcons (mcons 'a (mcons 'b '())) (mcons 'b '()))
(print-queue q1)
; '(a b)

(delete-queue! q1)
; (mcons (mcons 'b '()) (mcons 'b '()))
(print-queue q1)
; '(b)

(delete-queue! q1)
; (mcons '() (mcons 'b '()))
(print-queue q1)
; '()
