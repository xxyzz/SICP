# 4.1 The Metacircular Evaluator

## 4.1.1 The Core of the Evaluator

### Exercise 4.1:

Notice that we cannot tell whether the metacircular evaluator evaluates operands from left to right or from right to left. Its evaluation order is inherited from the underlying Lisp: If the arguments to `cons` in `list-of-values` are evaluated from left to right, then `list-of-values` will evaluate operands from left to right; and if the arguments to cons are evaluated from right to left, then `list-of-values` will evaluate operands from right to left.

Write a version of `list-of-values` that evaluates operands from left to right regardless of the order of evaluation in the underlying Lisp. Also write a version of `list-of-values` that evaluates operands from right to left.

## 4.1.2 Representing Expressions

### Exercise 4.2:

Louis Reasoner plans to reorder the `cond` clauses in `eval` so that the clause for procedure applications appears before the clause for assignments. He argues that this will make the interpreter more efficient: Since programs usually contain more applications than assignments, definitions, and so on, his modified `eval` will usually check fewer clauses than the original `eval` before identifying the type of an expression.

1. What is wrong with Louis’s plan? (Hint: What will Louis’s evaluator do with the expression `(define x 3)`?)

2. Louis is upset that his plan didn’t work. He is willing to go to any lengths to make his evaluator recognize procedure applications before it checks for most other kinds of expressions. Help him by changing the syntax of the evaluated language so that procedure applications start with `call`. For example, instead of `(factorial 3)` we will now have to write `(call factorial 3)` and instead of `(+ 1 2)` we will have to write `(call + 1 2)`.

### Exercise 4.3:

Rewrite `eval` so that the dispatch is done in data-directed style. Compare this with the data-directed differentiation procedure of Exercise 2.73. (You may use the `car` of a compound expression as the type of the expression, as is appropriate for the syntax implemented in this section.)

### Exercise 4.4:

Recall the definitions of the special forms `and` and `or` from Chapter 1:

- `and`: The expressions are evaluated from left to right. If any expression evaluates to false, false is returned; any remaining expressions are not evaluated. If all the expressions evaluate to true values, the value of the last expression is returned. If there are no expressions then true is returned.

- `or`: The expressions are evaluated from left to right. If any expression evaluates to a true value, that value is returned; any remaining expressions are not evaluated. If all expressions evaluate to false, or if there are no expressions, then false is returned.

Install `and` and `or` as new special forms for the evaluator by defining appropriate syntax procedures and evaluation procedures `eval-and` and `eval-or`. Alternatively, show how to implement `and` and `or` as derived expressions.

### Exercise 4.5:

Scheme allows an additional syntax for `cond` clauses, `(⟨test⟩ => ⟨recipient⟩)`. If `⟨test⟩` evaluates to a true value, then `⟨recipient⟩` is evaluated. Its value must be a procedure of one argument; this procedure is then invoked on the value of the `⟨test⟩`, and the result is returned as the value of the `cond` expression. For example

```scheme
(cond [(assoc 'b '((a 1) (b 2))) => cadr]
      [else #f])
```

returns 2. Modify the handling of cond so that it supports this extended syntax.

### Exercise 4.6:

`let` expressions are derived expressions, because

```scheme
(let ((⟨var1⟩ ⟨exp1⟩) . . . (⟨varn⟩ ⟨expn⟩)) ⟨body⟩)
```

is equivalent to

```scheme
((lambda (⟨var1⟩ ... ⟨varn⟩)
    ⟨body⟩)
  ⟨exp1⟩
  ...
  ⟨expn⟩)
```

Implement a syntactic transformation `let->combination` that reduces evaluating `let` expressions to evaluating combinations of the type shown above, and add the appropriate clause to `eval` to handle `let` expressions.

## Exercise 4.7:

`let*` is similar to `let`, except that the bindings of the `let*` variables are performed sequentially from left to right, and each binding is made in an environment in which all of the preceding bindings are visible. For example

```scheme
(let* ((x 3) (y (+ x 2)) (z (+ x y 5)))
  (* x z))
```

returns 39. Explain how a let* expression can be rewritten as a set of nested let expressions, and write a procedure `let*->nested-lets` that performs this transformation. If we have already implemented `let` (Exercise 4.6) and we want to extend the evaluator to handle `let*`, is it sufficient to add a clause to `eval` whose action is

```scheme
(eval (let*->nested-lets exp) env)
```

or must we explicitly expand `let*` in terms of non-derived expressions?

### Exercise 4.8:

“Named `let`” is a variant of `let` that has the form

```scheme
(let ⟨var⟩ ⟨bindings⟩ ⟨body⟩)
```

The ⟨bindings⟩ and ⟨body⟩ are just as in ordinary `let`, except that ⟨var⟩ is bound within ⟨body⟩ to a procedure whose body is ⟨body⟩ and whose parameters are the variables in the ⟨bindings⟩. Thus, one can repeatedly execute the ⟨body⟩ by invoking the procedure named ⟨var⟩. For example, the iterative Fibonacci procedure (Section 1.2.2) can be rewritten using named let as follows:

```scheme
(define (fib n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))
```

Modify `let->combination` of Exercise 4.6 to also support named `let`.

### Exercise 4.9:

Many languages support a variety of iteration constructs, such as `do`, `for`, `while`, and `until`. In Scheme, iterative processes can be expressed in terms of ordinary procedure calls, so special iteration constructs provide no essential gain in computational power. On the other hand, such constructs are often convenient. Design some iteration constructs, give examples of their use, and show how to implement them as derived expressions. 

### Exercise 4.10:

By using data abstraction, we were able to write an `eval` procedure that is independent of the particular syntax of the language to be evaluated. To illustrate this, design and implement a new syntax for Scheme by modifying the procedures in this section, without changing `eval` or `apply`.
