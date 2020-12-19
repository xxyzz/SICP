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
