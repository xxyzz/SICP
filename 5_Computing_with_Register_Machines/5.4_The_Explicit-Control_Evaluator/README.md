# 5.4 The Explicit-Control Evaluator

## 5.4.1 The Core of the Explicit-Control Evaluator

## 5.4.2 Sequence Evaluation and Tail Recursion

## 5.4.3 Conditionals, Assignments, and Definitions

### Exercise 5.23:

Extend the evaluator to handle derived expressions such as `cond`, `let`, and so on (Section 4.1.2). You may “cheat” and assume that the syntax transformers such as `cond->if` are available as machine operations.

### Exercise 5.24:

Implement `cond` as a new basic special form without reducing it to `if`. You will have to construct a loop that tests the predicates of successive `cond` clauses until you find one that is true, and then use `ev-sequence` to evaluate the actions of the clause.

### Exercise 5.25:

Modify the evaluator so that it uses normal-order evaluation, based on the lazy evaluator of Section 4.2.

## 5.4.4 Running the Evaluator

### Exercise 5.26:

Use the monitored stack to explore the tail-recursive property of the evaluator (Section 5.4.2). Start the evaluator and define the iterative `factorial` procedure from Section 1.2.1:

```scheme
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))
```

Run the procedure with some small values of *n*. Record the maximum stack depth and the number of pushes required to compute *n*! for each of these values.

a. You will find that the maximum depth required to evaluate *n*! is independent of *n*. What is that depth?

b. Determine from your data a formula in terms of *n* for the total number of push operations used in evaluating *n*! for any *n* > 1. Note that the number of operations used is a linear function of *n* and is thus determined by two constants. 
