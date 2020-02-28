#lang racket/base

(define (make-table same-key?)
  (let ([local-table (mcons '*table* null)])
    (define (assoc key records)
      (cond [(null? records) null]
            [(same-key? key (mcar (mcar records))) (mcar records)]
            [else (assoc key (mcdr records))]))
    (define (lookup key-1 key-2)
      (let ([subtable
             (assoc key-1 (mcdr local-table))])
        (if (not (null? subtable))
            (let ([record
                   (assoc key-2 (mcdr subtable))])
              (if (not (null? record))
                  (mcdr record)
                  null))
            null)))
    (define (insert! key-1 key-2 value)
      (let ([subtable
             (assoc key-1 (mcdr local-table))])
        (if (not (null? subtable))
            (let ([record
                   (assoc key-2 (mcdr subtable))])
              (if (not (null? record))
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                             (mcons (mcons key-2 value)
                                    (mcdr subtable)))))
            (set-mcdr! local-table
                       (mcons (mcons key-1 (mcons (mcons key-2 value) null))
                              (mcdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond [(eq? m 'lookup-proc) lookup]
            [(eq? m 'insert-proc!) insert!]
            [else (error "Unknown operation: TABLE" m)]))
    dispatch))

(define (same-key? key-1 key-2)
  (= (round key-1) (round key-2)))
(define operation-table (make-table same-key?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 1 1 "1-1")
; 'ok
(put 1 2 "1-2")
; 'ok
(put 2 1 "2-1")
; 'ok
(get 1.2 1)
; "1-1"
(get 1.6 2)
; '()
(get 2.3 1)
; "2-1"
(get 2 2)
; '()
