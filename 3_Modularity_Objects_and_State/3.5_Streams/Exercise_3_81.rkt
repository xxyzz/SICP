#lang racket/base
(require racket/stream)

(define (stream-map proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-map
              (cons proc (map stream-rest argstreams))))))

(define (random-stream input-stream)
  (stream-cons 0
               (stream-map (lambda (seed request)
                             (cond [(eq? (car request) 'generate) (random)]
                                   [(eq? (car request) 'reset)
                                    (random-seed seed)
                                    (cadr request)]
                                   [else (error "Unknown request: RANDOM-STREAM" request)]))
                           (random-stream input-stream)
                           input-stream)))

(define input1 (stream-cons '(generate) input1))
(define input2 (stream-cons '(reset 1) input1))

(stream->list (stream-take (random-stream input1) 5))
; '(0
;  0.4068513299862567
;  0.4505252190188611
;  0.2676094138675272
;  0.6582376395616283)
(stream->list (stream-take (random-stream input2) 5))
; '(0 1 0.8571568490678037 0.6594215608573717 0.205654820840853)
