#lang racket/base

(car ''abracadabra)
; 'quote

(cdr ''abracadabra)
; '(abracadabra)

(cadr ''abracadabra)
; 'abracadabra

(quote (quote abracadabra))
; ''abracadabra

(list 'quote 'abracadabra)
; ''abracadabra

'(a b c)
; '(a b c)

(quote (a b c))
; '(a b c)

(list 'a 'b 'c)
; '(a b c)