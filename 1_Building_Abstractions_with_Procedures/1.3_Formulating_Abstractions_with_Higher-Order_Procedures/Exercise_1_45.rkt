#lang racket/base
(require racket/function) ; identity

(define tolerance 0.00001)

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2))
            tolerance))
    (define (try guess)
        (let ([next (f guess)])
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

(define (average x y)
    (/ (+ x y) 2))

(define (average-damp f)
    (lambda (x) (average x (f x))))

(define (compose f g)
    (lambda (x) (f (g x))))

(define (repeated f n)
    (define (iter g i)
        (if (< i 1)
            g
            (iter (compose f g) (sub1 i))))
    (iter identity n))

(define (test n x damp-times)
    (fixed-point
        ((repeated average-damp damp-times) (lambda (y) (/ x (expt y (sub1 n)))))
        1.0))

(test 1 2 1)
; 1.9999923706054688

(test 2 2 1)
; 1.4142135623746899

(test 3 27 1)
; 2.9999972321057697

(test 4 16 2)
; 2.0000000000021965

(test 5 32 2)
; 2.000001512995761

(test 6 64 1)
; 1.9999945038915594

(test 7 2187 1)
; 2.9999957809730473

(test 8 256 1)
; 1.9999974160272682

(test 9 512 1)
; 11.9999990833911183

(test 10 1024 1)
; 1.999996695054027

(test 11 2048 1)
; 2.000007159580381

(test 12 4096 1)
; 2.0000019413977865

(test 13 8192 3)
; 2.0000029085658984

(test 14 16384 1)
; 1.9999973437067864

(test 15 32768 1)
; 2.0000054155274496

(test 16 2 1)
; 1.0442728200734304

(test 17 2 1)
; 1.0416125324325556

(test 18 2 1)
; 1.039259985129979

(test 19 2 1)
; 1.0371605641907835

(test 20 2 1)
; 1.0352608802309868

(test 21 2 1)
; 1.0335613676400777

(test 22 2 1)
; 1.032000714580566

(test 23 2 1)
; 1.0306033457564188

(test 24 2 1)
; 1.029301395073184

(test 25 2 2)
; 1.028106088691606

(test 26 2 1)
; 1.027018729092792

(test 27 2 1)
; 1.0260006725838586

(test 28 2 1)
; 1.0250583162788254

(test 29 2 1)
; 1.0241915789058758

(test 30 2 1)
; 1.0233744348921514

(test 31 2 1)
; 1.0226031770243056

(test 32 2 1)
; 1.0218925768781886

(test 32 4294967296 1)
; 1.9999999860301616

; don't know why it can't converge and don't know
; why averge one time works most of the time(but slow).
; TODO
