#lang racket/base

;; `assoc`(in `lookup-label`) returns the first matching element
;; labels of the dumb machine, the first `here` is used
;; ((controller
;;   {(goto (label here))}
;;   {(assign a (const 3))}
;;   {(goto (label there))}
;;   {(assign a (const 4))}
;;   {(goto (label there))})

;;  (start
;;   {(goto (label here))}
;;   {(assign a (const 3))}
;;   {(goto (label there))}
;;   {(assign a (const 4))}
;;   {(goto (label there))})

;;  (here
;;   {(assign a (const 3))}
;;   {(goto (label there))}
;;   {(assign a (const 4))}
;;   {(goto (label there))})

;;  (here
;;   {(assign a (const 4))}
;;   {(goto (label there))})

;;  (there))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ([next-inst (car text)])
           (if (symbol? next-inst)
               (begin
                 (duplicate-label? next-inst labels)
                 (receive insts
                   (cons (make-label-entry next-inst
                                           insts)
                         labels)))
               (receive (cons (make-instruction next-inst)
                              insts)
                   labels)))))))

(define (duplicate-label? label-name labels)
  (when (assoc label-name labels)
    (error "Duplicated label: ASSEMBLE"
           label-name)))

;; test
(define dumb-machine
  (make-machine
   '(a)
   null
   '(controller
     start
     (goto (label here))
     here
     (assign a (const 3))
     (goto (label there))
     here
     (assign a (const 4))
     (goto (label there))
     there)))
(start dumb-machine)
;; 'done
(get-register-contents dumb-machine 'a)
;; 3
