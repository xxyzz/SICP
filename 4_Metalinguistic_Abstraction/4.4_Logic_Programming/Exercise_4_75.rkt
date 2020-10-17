#lang racket/base
(require racket/stream)

(define (unique-query-content query) (car query))

(define (stream-singleton? stream)
  (and (not (stream-empty? stream))
       (stream-empty? (stream-rest stream))))

(define (uniquely-asserted query frame-stream)
  (stream-flatmap
   (lambda (frame)
     (let ([result-frames (qeval (unique-query-content query)
                                 (singleton-stream frame))])
       (if (stream-singleton? result-frames)
           result-frames
           empty-stream)))
   frame-stream))
(put 'unique 'qeval uniquely-asserted)

(and (address ?person ?ignore)
     (unique (supervisor ?x ?person)))
;;; Query results:
(and (address (Scrooge Eben) (Weston (Shady Lane) 10)) (unique (supervisor (Cratchet Robert) (Scrooge Eben))))
(and (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)) (unique (supervisor (Reasoner Louis) (Hacker Alyssa P))))
