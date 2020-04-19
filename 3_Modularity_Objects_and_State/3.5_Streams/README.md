# 3.5 Streams

## Exercise 3.50:

Complete the following definition, which generalizes `stream-map` to allow procedures that take multiple arguments, analogous to `map` in Section 2.2.1, Footnote 12.

```scheme
(define (stream-map proc . argstreams)
  (if (⟨??⟩ (car argstreams))
      the-empty-stream
      (⟨??⟩
       (apply proc (map ⟨??⟩ argstreams))
       (apply stream-map
              (cons proc (map ⟨??⟩ argstreams))))))
```

## Exercise 3.51:

In order to take a closer look at delayed evaluation, we will use the following procedure, which simply returns its argument after printing it:

```scheme
(define (show x)
  (display-line x)
  x)
```

What does the interpreter print in response to evaluating each expression in the following sequence?

```scheme
(define x
(stream-map show
(stream-enumerate-interval 0 10)))
(stream-ref x 5) (stream-ref x 7)
```

## Exercise 3.52:

Consider the sequence of expressions

```scheme
(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))
(stream-ref y 7)
(display-stream z)
```

What is the value of sum after each of the above expressions is evaluated? What is the printed response to evaluating the `stream-ref` and `display-stream` expressions? Would these responses differ if we had implemented `(delay ⟨exp⟩)` simply as `(lambda () ⟨exp⟩)` without using the optimization provided by `memo-proc`? Explain.

## Exercise 3.53:

Without running the program, describe the elements of the stream defined by

```scheme
(define s (cons-stream 1 (add-streams s s)))
```

## Exercise 3.54:

Define a procedure `mul-streams`, analogous to `add-streams`, that produces the elementwise product of its two input streams. Use this together with the stream of `integers` to complete the following definition of the stream whose n<sup>th</sup> element (counting from 0) is *n* + 1 factorial:

```scheme
(define factorials
  (cons-stream 1 (mul-streams ⟨??⟩ ⟨??⟩)))
```

## Exercise 3.55:

Define a procedure `partial-sums` that takes as argument a stream *S* and returns the stream whose elements are S<sub>0</sub>, S<sub>0</sub>+S<sub>1</sub>, S<sub>0</sub>+S<sub>1</sub>+S<sub>2</sub>, . . .. For example, `(partial-sums integers)` should be the stream 1, 3, 6, 10, 15, . . ..
