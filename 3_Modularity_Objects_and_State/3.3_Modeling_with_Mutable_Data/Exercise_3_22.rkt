#lang racket/base

(define (make-queue)
  (let ([front-ptr null]
        [rear-ptr null])
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (empty-queue?)
      (null? front-ptr))

    (define (queue)
      (cons front-ptr rear-ptr))

    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue" (queue))
          (mcar front-ptr)))

    (define (insert-queue! item)
      (let ([new-pair (mcons item '())])
        (cond [(empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               (queue)]
              [else
               (set-mcdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)
               (queue)])))

    (define (delete-queue!)
      (cond [(empty-queue?)
             (error "DELETE! called with an empty queue" (queue))]
            [else (set-front-ptr! (mcdr front-ptr))
                  (queue)]))

    (define (print-queue)
      (define (iter item)
        (when (not (null? item))
          (when (not (eq? (mcar item) (front-queue)))
            (display " "))
          (display (mcar item))
          (iter (mcdr item))))
      (display "'(")
      (iter front-ptr)
      (display ")\n"))

    (define (dispatch m)
      (cond [(eq? m 'set-front-ptr!) set-front-ptr!]
            [(eq? m 'set-rear-ptr!) set-rear-ptr!]
            [(eq? m 'empty-queue?) (empty-queue?)]
            [(eq? m 'front-queue) (front-queue)]
            [(eq? m 'insert-queue!) insert-queue!]
            [(eq? m 'delete-queue!) (delete-queue!)]
            [(eq? m 'print-queue) (print-queue)]
            [else (error "Unknown operation" m)]))
    dispatch))

(define q1 (make-queue))
((q1 'insert-queue!) 'a)
; (cons (mcons 'a '()) (mcons 'a '()))
(q1 'print-queue)
; '(a)

((q1 'insert-queue!) 'b)
; (cons (mcons 'a (mcons 'b '())) (mcons 'b '()))
(q1 'print-queue)
; '(a b)

(q1 'delete-queue!)
; (cons (mcons 'b '()) (mcons 'b '()))
(q1 'print-queue)
; '(b)

(q1 'delete-queue!)
; (cons '() (mcons 'b '()))
(q1 'print-queue)
; '()
