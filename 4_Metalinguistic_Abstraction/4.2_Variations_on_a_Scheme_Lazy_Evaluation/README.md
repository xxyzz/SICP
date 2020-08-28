# 4.2 Variations on a Scheme — Lazy Evaluation

## 4.2.1 Normal Order and Applicative Order

### Exercise 4.25:

Suppose that (in ordinary applicative-order Scheme) we define `unless` as shown above and then define `factorial` in terms of `unless` as

```
(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))
```

What happens if we attempt to evaluate `(factorial 5)`? Will our definitions work in a normal-order language?

### Exercise 4.26:

Ben Bitdiddle and Alyssa P. Hacker disagree over the importance of lazy evaluation for implementing things such as `unless`. Ben points out that it’s possible to implement unless in applicative order as a special form. Alyssa counters that, if one did that, `unless` would be merely syntax, not a procedure that could be used in conjunction with higher-order procedures. Fill in the details on both sides of the argument. Show how to implement `unless` as a derived expression (like `cond` or `let`), and give an example of a situation where it might be useful to have unless available as a procedure, rather than as a special form.

## 4.2.2 An Interpreter with Lazy Evaluation

### Exercise 4.27:

Suppose we type in the following definitions to the lazy evaluator:

```scheme
(define count 0)
(define (id x) (set! count (+ count 1)) x)
```

Give the missing values in the following sequence of interactions, and explain your answers.

```scheme
(define w (id (id 10)))
;;;  L-Eval input:
count
;;; L-Eval value:
⟨response⟩
;;; L-Eval input:
w
;;; L-Eval value:
⟨response⟩
;;; L-Eval input:
count
;;; L-Eval value:
⟨response⟩
```

### Exercise 4.28:

`eval` uses `actual-value` rather than `eval` to evaluate the operator before passing it to `apply`, in order to force the value of the operator. Give an example that demonstrates the need for this forcing.

### Exercise 4.29:

Exhibit a program that you would expect to run much more slowly without memoization than with memoization. Also, consider the following interaction, where the `id` procedure is defined as in Exercise 4.27 and `count` starts at 0:

```scheme
(define (square x) (* x x))
;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
⟨response⟩
;;; L-Eval input:
count
;;; L-Eval value:
⟨response⟩
```

Give the responses both when the evaluator memoizes and when it does not.

### Exercise 4.30:

Page 552.

### Exercise 4.31:

Page 554.

## 4.2.3 Streams as Lazy Lists

### Exercise 4.32:

Give some examples that illustrate the difference between the streams of Chapter 3 and the “lazier” lazy lists described in this section. How can you take advantage of this extra laziness?

As note 41 points out, we can build an infinite lazy tree and don't need to compute all the numbers.

### Exercise 4.33:

Ben Bitdiddle tests the lazy list implementation given above by evaluating the expression:

```scheme
(car '(a b c))
```

To his surprise, this produces an error. After some thought, he realizes that the “lists” obtained by reading in quoted expressions are different from the lists manipulated by the new definitions of `cons`, `car`, and `cdr`. Modify the evaluator’s treatment of quoted expressions so that quoted lists typed at the driver loop will produce true lazy lists.

### Exercise 4.34:

Modify the driver loop for the evaluator so that lazy pairs and lists will print in some reasonable way. (What are you going to do about infinite lists?) You may also need to modify the representation of lazy pairs so that the evaluator can identify them in order to print them.
