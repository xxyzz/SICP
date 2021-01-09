# 5.5 Compilation

## 5.5.1 Structure of the Compiler

### Exercise 5.31:

In evaluating a procedure application, the explicit-control evaluator always saves and restores the `env` register around the evaluation of the operator, saves and restores `env` around the evaluation of each operand (except the final one), saves and restores `argl` around the evaluation of each operand, and saves and restores `proc` around the evaluation of the operand sequence. For each of the following combinations, say which of these `save` and `restore` operations are superfluous and thus could be eliminated by the compiler’s `preserving` mechanism:

```scheme
(f 'x 'y)
((f) 'x 'y)
(f (g 'x) y)
(f (g 'x) 'y)
```

```
(f 'x 'y)
;; don't need to save or restore any register
;; find procedures is handles by lookup-variable-value, non register is involved
;; as mentioned in the book, parameters are self-evaluating quotes also don't any
;; register to eval

((f) 'x 'y)
;; nothing saved. `f` don't have operands and (f)'s operands are quotes
;; eventhough (f) when change `env` but quotes don't care

(f (g 'x) y)
;; save and resore `env`, `argl` when eval operand (g 'x)
;; save and resore `proc` when eval oprand list of `f`
;; other saves and restores are not needed

(f (g 'x) 'y)
;; save and resore `argl` when eval operand (g 'x), don't need `env` cuz
;; it won't affect 'y and `f`(f uses `env` when it's defined)
;; save and resore `proc` when eval oprand list of `f`
;; other saves and restores are not needed
```

### Exercise 5.32:

Using the `preserving` mechanism, the compiler will avoid saving and restoring `env` around the evaluation of the operator of a combination in the case where the operator is a symbol. We could also build such optimizations into the evaluator. Indeed, the explicit-control evaluator of Section 5.4 already performs a similar optimization, by treating combinations with no operands as a special case.

a. Extend the explicit-control evaluator to recognize as a separate class of expressions combinations whose operator is a symbol, and to take advantage of this fact in evaluating such expressions.

b. Alyssa P. Hacker suggests that by extending the evaluator to recognize more and more special cases we could incorporate all the compiler’s optimizations, and that this would eliminate the advantage of compilation altogether. What do you think of this idea?

## 5.5.2 Compiling Expressions

## 5.5.3 Compiling Combinations

## 5.5.4 Combining Instruction Sequences

## 5.5.5 An Example of Compiled Code

### Exercise 5.33:

Consider the following definition of a factorial procedure, which is slightly different from the one given above:

```scheme
(define (factorial-alt n)
  (if (= n 1)
    1
    (* n (factorial-alt (- n 1)))))
```

Compile this procedure and compare the resulting code with that produced for `factorial`. Explain any differences you find. Does either program execute more efficiently than the other?

### Exercise 5.34:

Compile the iterative factorial procedure

```scheme
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))
```

Annotate the resulting code, showing the essential difference between the code for iterative and recursive versions of `factorial` that makes one process build up stack space and the other run in constant stack space.

### Exercise 5.35:

What expression was compiled to produce the code shown in Figure 5.18?

```
  (assign val (op make-compiled-procedure) (label entry16)
                                           (reg env))
  (goto (label after-lambda15))
entry16
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env
          (op extend-environment) (const (x)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const g) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (const 2))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const x) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch19))
compiled-branch18
  (assign continue (label after-call17))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch19
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call17
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch22))
compiled-branch21
  (assign continue (label after-call20))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch22
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call20
  (assign argl (op list) (reg val))
  (restore env)
  (assign val (op lookup-variable-value) (const x) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch25))
compiled-branch24
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch25
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call23
after-lambda15
  (perform (op define-variable!) (const f) (reg val) (reg env))
  (assign val (const ok))
```

### Exercise 5.36:

What order of evaluation does our compiler produce for operands of a combination? Is it left-to-right, right-to-left, or some other order? Where in the compiler is this order determined? Modify the compiler so that it produces some other order of evaluation. (See the discussion of order of evaluation for the explicit-control evaluator in section 5.4.1.) How does changing the order of operand evaluation affect the efficiency of the code that constructs the argument list? 

### Exercise 5.37:

One way to understand the compiler's `preserving` mechanism for optimizing stack usage is to see what extra operations would be generated if we did not use this idea. Modify `preserving` so that it always generates the `save` and `restore` operations. Compile some simple expressions and identify the unnecessary stack operations that are generated. Compare the code to that generated with the `preserving` mechanism intact.

### Exercise 5.38:

Page 814

## 5.5.6 Lexical Addressing

### Exercise 5.39:

Write a procedure `lexical-address-lookup` that implements the new lookup operation. It should take two arguments—a lexical address and a run-time environment—and return the value of the variable stored at the specified lexical address. `lexical-address-lookup` should signal an error if the value of the variable is the symbol \*unassigned\*. Also write a procedure `lexical-address-set!` that implements the operation that changes the value of the variable at a specified lexical address.

### Exercise 5.40:

Modify the compiler to maintain the compile-time environment as described above. That is, add a compile-time-environment argument to `compile` and the various code generators, and extend it in `compile-lambda-body`.

### Exercise 5.41:

Write a procedure `find-variable` that takes as arguments a variable and a compile-time environment and returns the lexical address of the variable with respect to that environment. For example, in the program fragment that is shown above, the compile-time environment during the compilation of expression ⟨*e1*⟩ is `((y z) (a b c d e) (x y))`. `find-variable` should produce

```scheme
(find-variable 'c '((y z) (a b c d e) (x y)))
(1 2)
(find-variable 'x '((y z) (a b c d e) (x y)))
(2 0)
(find-variable 'w '((y z) (a b c d e) (x y)))
not-found
```

Already implemented in exercise 5.40

### Exercise 5.42:

Page 821

### Exercise 5.43:

We argued in Section 4.1.6 that internal definitions for block structure should not be considered "real" `defines`. Rather, a procedure body should be interpreted as if the internal variables being defined were installed as ordinary lambda variables initialized to their correct values using `set!`. Section 4.1.6 and Exercise 4.16 showed how to modify the metacircular interpreter to accomplish this by scanning out internal definitions. Modify the compiler to perform the same transformation before it compiles a procedure body.

### Exercise 5.44:

Page 822

## 5.5.7 Interfacing Compiled Code to the Evaluator

### Exercise 5.45:

Page 829


### Exercise 5.46:

Carry out an analysis like the one in Exercise 5.45 to determine the effectiveness of compiling the tree-recursive Fibonacci procedure

```scheme
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))
```

compared to the effectiveness of using the special-purpose Fibonacci machine of Figure 5.12. (For measurement of the interpreted performance, see Exercise 5.29.) For Fibonacci, the time resource used is not linear in *n*; hence the ratios of stack operations will not approach a limiting value that is independent of *n*.

### Exercise 5.47:

Page 831

### Exercise 5.48:

The `compile-and-go` interface implemented in this section is awkward, since the compiler can be called only once (when the evaluator machine is started). Augment the compiler-interpreter interface by providing a `compile-and-run` primitive that can be called from within the explicit-control evaluator as follows:

```scheme
;;; EC-Eval input:
(compile-and-run
  '(define (factorial n)
    (if (= n 1) 1 (* (factorial (- n 1)) n))))
;;; EC-Eval value:
ok
;;; EC-Eval input:
(factorial 5)
;;; EC-Eval value:
120
```

### Exercise 5.49:

As an alternative to using the explicit-control evaluator’s read-eval-print loop, design a register machine that performs a read-compile-execute-print loop. That is, the machine should run a loop that reads an expression, compiles it, assembles and executes the resulting code, and prints the result. This is easy to run in our simulated setup, since we can arrange to call the procedures `compile` and `assemble` as "register-machine operations."

### Exercise 5.50:

Use the compiler to compile the metacircular evaluator of Section 4.1 and run this program using the register-machine simulator. (To compile more than one definition at a time, you can package the definitions in a begin.) The resulting interpreter will run very slowly because of the multiple levels of interpretation, but getting all the details to work is an instructive exercise.
