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

TODO darn

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

### Exercise 5.27:

For comparison with Exercise 5.26, explore the behavior of the following procedure for computing factorials recursively:

```scheme
(define (factorial n)
  (if (= n 1) 1 (* (factorial (- n 1)) n)))
```

By running this procedure with the monitored stack, determine, as a function of *n*, the maximum depth of the stack and the total number of pushes used in evaluating *n*! for *n* ≥ 1. (Again, these functions will be linear.) Summarize your experiments by filling in the following table with the appropriate expressions in terms of *n*:

|                     | Maximum depth | Number of pushes |
|---------------------|---------------|------------------|
| Recursive factorial | 5 * n + 3     | 32 * n - 16      |
| Iterative factorial | 10            | 35 * n + 29      |

The maximum depth is a measure of the amount of space used by the evaluator in carrying out the computation, and the number of pushes correlates well with the time required.

```scheme
;;EC-Eval input:
(factorial 1)

(total-pushes = 16 maximum-depth = 8)

;;EC-Eval value:
1

;;EC-Eval input:
(factorial 2)

(total-pushes = 48 maximum-depth = 13)

;;EC-Eval value:
2

;;EC-Eval input:
(factorial 3)

(total-pushes = 80 maximum-depth = 18)

;;EC-Eval value:
6

;;EC-Eval input:
(factorial 4)

(total-pushes = 112 maximum-depth = 23)

;;EC-Eval value:
24
```

### Exercise 5.28:

Modify the definition of the evaluator by changing `eval-sequence` as described in Section 5.4.2 so that the evaluator is no longer tail-recursive. Rerun your experiments from Exercise 5.26 and Exercise 5.27 to demonstrate that both versions of the `factorial` procedure now require space that grows linearly with their input.

```scheme
;; Iterative factorial:
;;EC-Eval input:
(factorial 1)

(total-pushes = 70 maximum-depth = 17)

;;EC-Eval value:
1

;;EC-Eval input:
(factorial 2)

(total-pushes = 107 maximum-depth = 20)

;;EC-Eval value:
2

;;EC-Eval input:
(factorial 3)

(total-pushes = 144 maximum-depth = 23)

;;EC-Eval value:
6

;;EC-Eval input:
(factorial 4)

(total-pushes = 181 maximum-depth = 26)

;;EC-Eval value:
24

;; Recursive factorial:
;;EC-Eval input:
(factorial 1)

(total-pushes = 18 maximum-depth = 11)

;;EC-Eval value:
1

;;EC-Eval input:
(factorial 2)

(total-pushes = 52 maximum-depth = 19)

;;EC-Eval value:
2

;;EC-Eval input:
(factorial 3)

(total-pushes = 86 maximum-depth = 27)

;;EC-Eval value:
6

;;EC-Eval input:
(factorial 4)

(total-pushes = 120 maximum-depth = 35)

;;EC-Eval value:
24
```

|                     | Maximum depth | Number of pushes |
|---------------------|---------------|------------------|
| Recursive factorial | 8 * n + 3     | 34 * n - 16      |
| Iterative factorial | 3 * n + 14    | 37 * n + 33      |
