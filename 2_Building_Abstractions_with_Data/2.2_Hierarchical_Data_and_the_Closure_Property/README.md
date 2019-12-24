# 2.2 Hierarchical Data and the Closure Property

## Exercise 2.17:

Define a procedure `last-pair` that returns the list that contains only the last element of a given (nonempty) list:

```scheme
(last-pair (list 23 72 149 34))
; (34)
```

## Exercise 2.18:

Define a procedure `reverse` that takes a list as argument and returns a list of the same elements in reverse order:

```scheme
(reverse (list 1 4 9 16 25))
; (25 16 9 4 1)
```

## Exercise 2.19:

Consider the change-counting program of Section 1.2.2. It would be nice to be able to easily change the currency used by the program, so that we could compute the number of ways to change a British pound, for example. As the program is written, the knowledge of the currency is distributed partly into the procedure `first-denomination` and partly into the procedure `count-change` (which knows that there are five kinds of U.S. coins). It would be nicer to be able to supply a list of coins to be used for making change.

We want to rewrite the procedure cc so that its second argument is a list of the values of the coins to use rather than an integer specifying which coins to use. We could then have lists that defined each kind of currency:

```scheme
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
```

We could then call `cc` as follows:

```scheme
(cc 100 us-coins)
; 292
```

To do this will require changing the program `cc` somewhat. It will still have the same form, but it will access its second argument differently, as follows:

```scheme
(define (cc amount coin-values)
    (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
            (+ (cc amount
                    (except-first-denomination coin-values))
                (cc (- amount (first-denomination coin-values))
                    coin-values)))))
```

Define the procedures `first-denomination`, `except-first-denomination`, and `no-more?` in terms of primitive operations on list structures. Does the order of the list `coin-values` affect the answer produced by `cc`? Why or why not?

## Exercise 2.20:

The procedures`+`, ``*`, and `list` take arbitrary numbers of arguments. One way to define such procedures is to use `define` with *dotted-tail notation*. In a procedure definition, a parameter list that has a dot before the last parameter name indicates that, when the procedure is called, the initial parameters (if any) will have as values the initial arguments, as usual, but the final parameter’s value will be a *list* of any remaining arguments. For instance, given the definition

```scheme
(define (f x y . z) ⟨body⟩)
```

Use this notation to write a procedure `same-parity` that takes one or more integers and returns a list of all the arguments that have the same even-odd parity as the first argument. For example,

```scheme
(same-parity 1 2 3 4 5 6 7)
; (1 3 5 7)
(same-parity 2 3 4 5 6 7)
; (2 4 6)
```

## Exercise 2.21:

The procedure `square-list` takes a list of numbers as argument and returns a list of the squares of those numbers.

```scheme
(square-list (list 1 2 3 4))
; (1 4 9 16)
```

Here are two different definitions of `square-list`. Complete both of them by filling in the missing expressions:

```scheme
(define (square-list items)
    (if (null? items)
        nil
        (cons ⟨??⟩ ⟨??⟩)))
(define (square-list items)
    (map ⟨??⟩ ⟨??⟩))
```

## Exercise 2.22:

Louis Reasoner tries to rewrite the first `square-list` procedure of Exercise 2.21 so that it evolves an iterative process:

```scheme
(define (square-list items)
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things)
                (cons (square (car things))
                      answer))))
    (iter items nil))
```

Unfortunately, defining `square-list` this way produces the answer list in the reverse order of the one desired. Why?

Louis then tries to fix his bug by interchanging the arguments to `cons`:

```scheme
(define (square-list items)
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things)
                (cons answer
                      (square (car things))))))
    (iter items nil))
```

This doesn’t work either. Explain.

The first version add squared value to the front, so the result list is reversed.

The second version is filed because of `cons` works this way:

```scheme
(cons 1 (list 2 3))
; '(1 2 3)
(cons (list 1 2) 3)
; '((1 2) . 3)
```

## Exercise 2.23:

The procedure `for-each` is similar to `map`. It takes as arguments a procedure and a list of elements. However, rather than forming a list of the results, `for-each` just applies the procedure to each of the elements in turn, from left to right. The values returned by applying the procedure to the elements are not used at all—`for-each` is used with procedures that perform an action, such as printing. For example,

```scheme
(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))

; 57
; 321
; 88
```

The value returned by the call to `for-each` (not illustrated above) can be something arbitrary, such as true. Give an implementation of `for-each`.

## Exercise 2.24

Suppose we evaluate the expression `(list 1 (list 2 (list 3 4)))`. Give the result printed by the interpreter, the corresponding box-and-pointer structure, and the interpretation of this as a tree (as in Figure 2.6).

## Exercise 2.25:

Give combinations of `car`s and `cdr`s that will pick 7 from each of the following lists:

```scheme
(1 3 (5 7) 9)
((7))
(1 (2 (3 (4 (5 (6 7))))))
```

## Exercise 2.26:

Suppose we define `x` and `y` to be two lists:

```scheme
(define x (list 1 2 3))
(define y (list 4 5 6))
```

What result is printed by the interpreter in response to evaluating each of the following expressions:

```scheme
(append x y)
(cons x y)
(list x y)
```

## Exercise 2.27:

Modify your reverse procedure of Exercise 2.18 to produce a `deep-reverse` procedure that takes a list as argument and returns as its value the list with its elements reversed and with all sublists `deep-reversed` as well. For example,

```scheme
(define x (list (list 1 2) (list 3 4)))
x
; '((1 2) (3 4))
(reverse x)
; '((3 4) (1 2))
(deep-reverse x)
; '((4 3) (2 1))
```
