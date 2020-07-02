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

### Exercise 4.7:

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

## 4.1.3 Evaluator Data Structures

### Exercise 4.11:

Instead of representing a frame as a pair of lists, we can represent a frame as a list of bindings, where each binding is a name-value pair. Rewrite the environment operations to use this alternative representation.

### Exercise 4.12:

The procedures `set-variable-value!`, `define-variable!` and `lookup-variable-value` can be expressed in terms of more abstract procedures for traversing the environment structure. Define abstractions that capture the common patterns and redefine the three procedures in terms of these abstractions.

### Exercise 4.13:

Scheme allows us to create new bindings for variables by means of `define`, but provides no way to get rid of bindings. Implement for the evaluator a special form `make-unbound!` that removes the binding of a given symbol from the environment in which the `make-unbound!` expression is evaluated. This problem is not completely specified. For example, should we remove only the binding in the first frame of the environment? Complete the specification and justify any choices you make.

## 4.1.4 Running the Evaluator as a Program

### Exercise 4.14:

Eva Lu Ator and Louis Reasoner are each experimenting with the metacircular evaluator. Eva types in the definition of `map`, and runs some test programs that use it. They work fine. Louis, in contrast, has installed the system version of `map` as a primitive for the metacircular evaluator. When he tries it, things go terribly wrong. Explain why Louis’s `map` fails even though Eva’s works.

In our implication of Scheme, a procedure is a list which the first element is an object tells what it is. `(map (lambda (x) (+ 1 x)) (list 1 2))` will transform to `(apply map (list (list 'procedure (list 'x) (+ 1 x) env) (list 1 2)))`, the Scheme implication `map` will complain that's not a valid procedure.

## 4.1.5 Data as Programs

### Exercise 4.15:

Given a one-argument procedure `p` and an object `a`, `p` is said to “halt” on `a` if evaluating the expression `(p a)` returns a value (as opposed to terminating with an error message or running forever). Show that it is impossible to write a procedure `halts?` that correctly determines whether `p` halts on `a` for any procedure `p` and object `a`. Use the following reasoning: If you had such a procedure `halts?`, you could implement the following program:

```scheme
(define (run-forever) (run-forever))
(define (try p)
  (if (halts? p p) (run-forever) 'halted))
```

Now consider evaluating the expression `(try try)` and show that any possible outcome (either halting or running forever) violates the intended behavior of `halts?`.

Assume `try` runs forever then `try` will return `'halted`. If `try` returns `'halted` it will run forever. Thus `halts?` can't exits.

[Halting problem](https://en.wikipedia.org/wiki/Halting_problem#Proof_concept)

## 4.1.6 Internal Definitions

### Exercise 4.16:

In this exercise we implement the method just described for interpreting internal definitions. We assume that the evaluator supports `let` (see Exercise 4.6).

a. Change `lookup-variable-value` (Section 4.1.3) to signal an error if the value it finds is the symbol `*unassigned*`.

b. Write a procedure `scan-out-defines` that takes a procedure body and returns an equivalent one that has no internal definitions, by making the transformation described above.

c. Install `scan-out-defines` in the interpreter, either in `make-procedure` or in `procedure-body` (see Section 4.1.3). Which place is better? Why?

### Exercise 4.17:

Draw diagrams of the environment in effect when evaluating the expression ⟨e3⟩ in the procedure in the text, comparing how this will be structured when definitions are interpreted sequentially with how it will be structured if definitions are scanned out as described. Why is there an extra frame in the transformed program? Explain why this difference in environment structure can never make a difference in the behavior of a correct program. Design a way to make the interpreter implement the “simultaneous” scope rule for internal definitions without constructing the extra frame.

Add `(define var '*unassigned*)` and `(set! var value)` to the beginning of the body.

### Exercise 4.18:

Consider an alternative strategy for scanning out definitions that translates the example in the text to

```scheme
(lambda ⟨vars⟩
  (let ((u '*unassigned*) (v '*unassigned*))
    (let ((a ⟨e1⟩) (b ⟨e2⟩))
      (set! u a)
      (set! v b))
    ⟨e3⟩))
```

Here `a` and `b` are meant to represent new variable names, created by the interpreter, that do not appear in the user’s program. Consider the `solve` procedure from Section 3.5.4:

```scheme
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)
```

Will this procedure work if internal definitions are scanned out as shown in this exercise? What if they are scanned out as shown in the text? Explain.

```scheme
;; 4.18 not work
(define (solve f y0 dt)
  (let ((y '*unassigned) (dy '*unassigned))
    (let ((a (integral (delay dy) y0 dt)) (b (stream-map f y))) ;; y is *unassigned* here
      (set! y a)
      (set! dy b)
      y)))

;; 4.16 this works
(define (solve f y0 dt)
  (let ((y '*unassigned) (dy '*unassigned))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (stream-map f y))
    y))
```

### Exercise 4.19:

Ben Bitdiddle, Alyssa P. Hacker, and Eva Lu Ator are arguing about the desired result of evaluating the expression

```scheme
(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))
```

Ben asserts that the result should be obtained using the sequential rule for `define`: `b` is defined to be 11, then `a` is defined to be 5, so the result is 16. Alyssa objects that mutual recursion requires the simultaneous scope rule for internal procedure definitions, and that it is unreasonable to treat procedure names differently from other names. Thus, she argues for the mechanism implemented in Exercise 4.16. This would lead to `a` being unassigned at the time that the value for `b` is to be computed. Hence, in Alyssa’s view the procedure should produce an error. Eva has a third opinion. She says that if the definitions of `a` and `b` are truly meant to be simultaneous, then the value 5 for a should be used in evaluating `b`. Hence, in Eva’s view `a` should be 5, `b` should be 15, and the result should be 20. Which (if any) of these viewpoints do you support? Can you devise a way to implement internal definitions so that they behave as Eva prefers?

In Racket, `a` is undefined at line 3. I'm with Alyssa and Racket. If `a` and `b` don't reference each other, rearrange the code will solve the problem otherwise throw an error.

### Exercise 4.20:

Because internal definitions look sequential but are actually simultaneous, some people prefer to avoid them entirely, and use the special form `letrec` instead. `letrec` looks like `let`, so it is not surprising that the variables it binds are bound simultaneously and have the same scope as each other. The sample procedure `f` above can be written without internal definitions, but with exactly the same meaning, as

```scheme
(define (f x)
  (letrec
    ((even? (lambda (n)
              (if (= n 0) true (odd? (- n 1)))))
     (odd? (lambda (n)
              (if (= n 0) false (even? (- n 1))))))
    ⟨rest of body of f⟩))
```

`letrec` expressions, which have the form

```scheme
(letrec ((⟨var1⟩ ⟨exp1⟩) . . . (⟨varn⟩ ⟨expn⟩))
  ⟨body⟩)
```

are a variation on `let` in which the expressions ⟨exp<sub>k</sub>⟩ that provide the initial values for the variables ⟨var<sub>k</sub>⟩ are evaluated in an environment that includes all the `letrec` bindings. This permits recursion in the bindings, such as the mutual recursion of `even?` and `odd?` in the example above, or the evaluation of 10 factorial with

```scheme
(letrec
  ((fact (lambda (n)
           (if (= n 1) 1 (* n (fact (- n 1)))))))
  (fact 10))
```

a. Implement `letrec` as a derived expression, by transforming a `letrec` expression into a `let` expression as shown in the text above or in Exercise 4.18. That is, the `letrec` variables should be created with a `let` and then be assigned their values with `set!`.

b. Louis Reasoneris confused by all this fuss about internal definitions. The way he sees it, if you don’t like to use `define` inside a procedure, you can just use `let`. Illustrate what is loose about his reasoning by drawing an environment diagram that shows the environment in which the ⟨rest of body of `f`⟩ is evaluated during evaluation of the expression `(f 5)`, with `f` defined as in this exercise. Draw an environment diagram for the same evaluation, but with `let` in place of `letrec` in the definition of `f`.

### Exercise 4.21:

Amazingly, Louis’s intuition in Exercise 4.20 is correct. It is indeed possible to specify recursive procedures without using `letrec` (or even `define`), although the method for accomplishing this is much more subtle than Louis imagined. The following expression computes 10 factorial by applying a recursive factorial procedure:

```scheme
((lambda (n)
  ((lambda (fact) (fact fact n))
   (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))))
  10)
```

a. Check (by evaluating the expression) that this really does compute factorials. Devise an analogous expression for computing Fibonacci numbers.

b. Consider the following procedure, which includes mutually recursive internal definitions:

```scheme
(define (f x)
  (define (even? n)
    (if (= n 0) true (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0) false (even? (- n 1))))
  (even? x))
```

Fill in the missing expressions to complete an alternative definition of `f`, which uses neither internal definitions nor `letrec`:

```scheme
(define (f x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ⟨??⟩ ⟨??⟩ ⟨??⟩)))
   (lambda (ev? od? n)
    (if (= n 0) false (ev? ⟨??⟩ ⟨??⟩ ⟨??⟩)))))
```

## 4.1.7 Separating Syntactic Analysis from Execution

### Exercise 4.22:

Extend the evaluator in this section to support the special form `let`. (See Exercise 4.6.)
