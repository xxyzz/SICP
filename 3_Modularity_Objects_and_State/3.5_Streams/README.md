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

## Exercise 3.56:

Page 448.

## Exercise 3.57:

How many additions are performed when we compute the n<sup>th</sup> Fibonacci number using the definition of fibs based on the add-streams procedure? Show that the number of additions would be exponentially greater if we had implemented `(delay ⟨exp⟩)` simply as `(lambda () ⟨exp⟩)`, without using the optimization provided by the `memo-proc` procedure described in Section 3.5.1.

## Exercise 3.58:

Give an interpretation of the stream computed by the following procedure:

```scheme
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))
```

(`quotient` is a primitive that returns the integer quotient of two integers.) What are the successive elements produced by `(expand 1 7 10)`? What is produced by `(expand 3 8 10)`?

## Exercise 3.59:

Page 450.

## Exercise 3.60:

With power series represented as streams of coefficients as in Exercise 3.59, adding series is implemented by `add-streams`. Complete the definition of the following procedure for multiplying series:

```scheme
(define (mul-series s1 s2)
  (cons-stream ⟨??⟩ (add-streams ⟨??⟩ ⟨??⟩)))
```

You can test your procedure by verifying that sin<sup>2</sup>x + cos<sup>2</sup>x = 1, using the series from Exercise 3.59.

## Exercise 3.61:

Let S be a power series (Exercise 3.59) whose constant term is 1. Suppose we want to find the power series 1/S, that is, the series X such that SX = 1. Write S = 1+S<sub>R</sub> where S<sub>R</sub> is the part of S after the constant term. Then we can solve for X as follows:

S·X = 1,

(1 + S<sub>R</sub>)·X = 1,

X + S<sub>R</sub>·X = 1,

X = 1 - S<sub>R</sub>·X.

In other words, X is the power series whose constant term is 1 and whose higher-order terms are given by the negative of S<sub>R</sub> times X. Use this idea to write a procedure `invert-unit-series` that computes 1/S for a power series S with constant term 1. You will need to use `mul-series` from Exercise 3.60.

## Exercise 3.62:

Use the results of Exercise 3.60 and Exercise 3.61 to define a procedure `div-series` that divides two power series. `div-series` should work for any two series, provided that the denominator series begins with a nonzero constant term. (If the denominator has a zero constant term, then div-series should signal an error.) Show how to use `div-series` together with the result of Exercise 3.59 to generate the power series for tangent.

## Exercise 3.63:

Louis Reasoner asks why the `sqrt-stream `procedure was not written in the following more straightforward way, without the local variable `guesses`:

```scheme
(define (sqrt-stream x) 
  (cons-stream 1.0 (stream-map
                    (lambda (guess)
                      (sqrt-improve guess x))
                    (sqrt-stream x))))
```

Alyssa P. Hacker replies that this version of the procedure is considerably less efficient because it performs redundant computation. Explain Alyssa’s answer. Would the two versions still differ in efficiency if our implementation of delay used only `(lambda () ⟨exp⟩)` without using the optimization provided by `memo-proc` (Section 3.5.1)?

## Exercise 3.64:

Write a procedure `stream-limit` that takes as arguments a stream and a number (the tolerance). It should examine the stream until it finds two successive elements that differ in absolute value by less than the tolerance, and return the second of the two elements. Using this, we could compute square roots up to a given tolerance by

```scheme
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
```

## Exercise 3.65:

Use the series

In 2 = 1 - 1/2 + 1/3 - 1/4 + ...

to compute three sequences of approximations to the natural logarithm of 2, in the same way we did above for *π*. How rapidly do these sequences converge?

## Exercise 3.66:

Examine the stream `(pairs integers integers)`. Can you make any general comments about the order in which the pairs are placed into the stream? For example, approximately how many pairs precede the pair (1, 100)? the pair (99, 100)? the pair (100, 100)? (If you can make precise mathematical statements here, all the better. But feel free to give more qualitative answers if you find yourself getting bogged down.)
