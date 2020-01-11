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
