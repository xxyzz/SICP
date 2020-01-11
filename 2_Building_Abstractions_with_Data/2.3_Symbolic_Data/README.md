# 2.3 Symbolic Data

## Exercise 2.53:

What would the interpreter print in response to evaluating each of the following expressions?

```scheme
(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))
```

## Exercise 2.54:

Two lists are said to be `equal?` if they contain equal elements arranged in the same order. For example,

```scheme
(equal? '(this is a list) '(this is a list))
```

is true, but

```scheme
(equal? '(this is a list) '(this (is a) list))
```

is false. To be more precise, we can define `equal?` recursively in terms of the basic `eq?` equality of symbols by saying that a and b are `equal?` if they are both symbols and the symbols are `eq?`, or if they are both lists such that `(car a)` is `equal?` to `(car b)` and `(cdr a)` is `equal?` to `(cdr b)`. Using this idea, implement `equal?` as a procedure.

## Exercise 2.55:

Eva Lu Ator types to the interpreter the expression

```scheme
(car ''abracadabra)
```

To her surprise, the interpreter prints back `quote`. Explain.

## Exercise 2.56:

Show how to extend the basic differentiator to handle more kinds of expressions. For instance, implement the differentiation rule

![\frac{\mathrm{d} (u^{n})}{\mathrm{d} x} = nu^{n-1}\frac{\mathrm{d} u}{\mathrm{d} x}](https://quicklatex.com/cache3/97/ql_c2546a587bd5234c3ed6065f5ef25297_l3.png)

by adding a new clause to the `deriv` program and defining appropriate procedures `exponentiation?`, `base`, `exponent`, and `make-exponentiation`. (You may use the symbol ** to denote exponentiation.) Build in the rules that anything raised to the power 0 is 1 and anything raised to the power 1 is the thing itself.

## Exercise 2.57:

Extend the differentiation program to handle sums and products of arbitrary numbers of (two or more) terms. Then the last example above could be expressed as

```scheme
(deriv '(* x y (+ x 3)) 'x)
```
Try to do this by changing only the representation for sums and products, without changing the deriv procedure at all. For example, the addend of a sum would be the first term, and the augend would be the sum of the rest of the terms.
