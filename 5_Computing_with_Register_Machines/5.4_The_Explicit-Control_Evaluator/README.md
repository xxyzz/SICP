# 5.4 The Explicit-Control Evaluator

## 5.4.1 The Core of the Explicit-Control Evaluator

## 5.4.2 Sequence Evaluation and Tail Recursion

## 5.4.3 Conditionals, Assignments, and Definitions

### Exercise 5.23:

Extend the evaluator to handle derived expressions such as `cond`, `let`, and so on (Section 4.1.2). You may “cheat” and assume that the syntax transformers such as `cond->if` are available as machine operations.

### Exercise 5.24:

Implement `cond` as a new basic special form without reducing it to `if`. You will have to construct a loop that tests the predicates of successive `cond` clauses until you find one that is true, and then use `ev-sequence` to evaluate the actions of the clause.
