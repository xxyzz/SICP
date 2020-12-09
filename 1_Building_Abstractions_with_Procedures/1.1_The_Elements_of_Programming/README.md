# 1.1 The Elements of Programming

### Exercise 1.1:

Below is a sequence of expressions. What is the result printed by the interpreter in response to each expression? Assume that the sequence is to be evaluated in the order in which it is presented.

```scheme
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))

(+ 2 (if (> b a) b a))

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
```

### Exercise 1.2:

Translate the following expression into prefix form:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{5&plus;4&plus;(2-(3-(6&plus;\frac{4}{5})))}{3(6-2)(2-7)))}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{5&plus;4&plus;(2-(3-(6&plus;\frac{4}{5})))}{3(6-2)(2-7)))}" title="\frac{5+4+(2-(3-(6+\frac{4}{5})))}{3(6-2)(2-7)))}" /></a>

### Exercise 1.3:

Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.

### Exercise 1.4:

Observe that our model of evaluation allows for combinations whose operators are compound expressions. Use this observation to describe the behavior of the following procedure:

```scheme
(define (a-plus-abs-b a b)
    ((if (> b 0) + -) a b))
```

As the procedure name indicates: a plus the absolute value of b.

### Exercise 1.5:

Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using applicative-order evaluation or normal-order evaluation. He defines the following two procedures:

```scheme
(define (p) (p))
(define (test x y)
    (if (= x 0) 0 y))
```

Then he evaluates the expression

```scheme
(test 0 (p))
```

What behavior will Ben observe with an interpreter that uses applicative-order evaluation? What behavior will he observe with an interpreter that uses normal-order evaluation? Explain your answer. (Assume that the evaluation rule for the special form if is the same whether the interpreter is using normal or applicative order: The predicate expression is evaluated first, and the result determines whether to evaluate the consequent or the alternative expression.)

Applicative-order: infinite loop inside p procedure

Normal-order evaluation: 0

### Exercise 1.6:

Alyssa P. Hacker doesn’t see why `if` needs to be provided as a special form. “Why can’t I just define it as an ordinary procedure in terms of `cond`?” she asks. Alyssa’s friend Eva Lu Ator claims this can indeed be done, and she defines a new version of `if`:

```scheme
(define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
    (else else-clause)))
```

Eva demonstrates the program for Alyssa:

```scheme
(new-if (= 2 3) 0 5)
; 5
(new-if (= 1 1) 0 5)
; 0
```

Delighted, Alyssa uses `new-if` to rewrite the square-root program:

```scheme
(define (sqrt-iter guess x)
    (new-if (good-enough? guess x)
        guess
    (sqrt-iter (improve guess x) x)))
```

What happens when Alyssa attempts to use this to compute square roots? Explain.

```scheme
(if ⟨predicate⟩ ⟨consequent⟩ ⟨alternative⟩)

(define (⟨name⟩ ⟨formal parameters⟩)
    ⟨body⟩)
```

The special form `if` evaluates the predicate first then evaluates the consequent or the alternative. The procedure evaluates the parameters first then evaluates the body. So the `new-if` procedure will cause the program iterates forever inside `sqrt-iter`.

### Exercise 1.7:

The `good-enough?` test used in computing square roots will not be very effective for finding the square roots of very small numbers. Also, in real computers, arithmetic operations are almost always performed with limited precision. This makes our test inadequate for very large numbers. Explain these statements, with examples showing how the test fails for small and large numbers. An alternative strategy for implementing `good-enough?` is to watch how `guess` changes from one iteration to the next and to stop when the change is a very small fraction of the `guess`. Design a square-root procedure that uses this kind of end test. Does this work better for small and large numbers?

### Exercise 1.8:

Newton’s method for cube roots is based on the fact that if `y` is an approximation to the cube root of `x`, then a better approximation is given by the value

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\frac{x}{^{y2}}&plus;2y}{3}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\frac{x}{^{y2}}&plus;2y}{3}" title="\frac{\frac{x}{^{y2}}+2y}{3}" /></a>

Use this formula to implement a cube-root procedure analogous to the square-root procedure. (In Section 1.3.4 we will see how to implement Newton’s method in general as an abstraction of these square-root and cube-root procedures.)
