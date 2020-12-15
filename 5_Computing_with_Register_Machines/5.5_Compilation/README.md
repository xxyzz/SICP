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
