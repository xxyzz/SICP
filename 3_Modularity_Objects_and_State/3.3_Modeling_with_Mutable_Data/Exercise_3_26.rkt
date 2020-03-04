#lang racket/base

(define (make-table)
  (define (get-key tree) (mcar tree))
  (define (get-value tree) (mcar (mcdr tree)))
  (define (left-branch tree) (mcar (mcdr (mcdr tree))))
  (define (right-branch tree) (mcar (mcdr (mcdr (mcdr tree)))))
  (define (make-tree key value left right)
    (mcons key (mcons value (mcons left (mcons right null)))))
  (define (set-value! tree value)
    (set-mcar! (mcdr tree) value))
  (define (set-left-branch! tree branch)
    (set-mcar! (mcdr (mcdr tree)) branch))
  (define (set-right-branch! tree branch)
    (set-mcar! (mcdr (mcdr (mcdr tree))) branch))

  (let ([local-table (mcons '*table* (make-tree null null null null))])
    (define (lookup key)
      (define (iter-lookup tree)
        (cond [(null? tree) null]
              [(= key (get-key tree)) (get-value tree)]
              [(< key (get-key tree))
               (iter-lookup (left-branch tree))]
              [(> key (get-key tree))
               (iter-lookup (right-branch tree))]))
      (iter-lookup (mcdr local-table)))

    (define (insert! key value)
      (define (iter-insert! tree)
        (cond [(null? tree) null]
              [(null? (get-value tree))
               (set-mcar! tree key)
               (set-value! tree value)]
              [(= key (get-key tree)) (set-value! tree value)]
              [(< key (get-key tree))
               (if (null? (left-branch tree))
                   (set-left-branch! tree (make-tree key value null null))
                   (iter-insert! (left-branch tree)))]
              [(> key (get-key tree))
               (if (null? (right-branch tree))
                   (set-right-branch! tree (make-tree key value null null))
                   (iter-insert! (right-branch tree)))]))
      (iter-insert! (mcdr local-table))
      'ok)

    (define (dispatch m)
      (cond [(eq? m 'lookup-proc) lookup]
            [(eq? m 'insert-proc!) insert!]
            [else (error "Unknown operation: TABLE" m)]))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 1 1)
; 'ok
(put 2 2)
; 'ok
(put 3 3)
; 'ok
(put 4 4)
; 'ok
(get 1)
; 1
(get 2)
; 2
(get 3)
; 3
(get 4)
; 4
(get 5)
; '()
