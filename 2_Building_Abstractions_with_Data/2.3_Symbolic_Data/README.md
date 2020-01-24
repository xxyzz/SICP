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

## Exercise 2.58:

Suppose we want to modify the differentiation program so that it works with ordinary mathematical notation, in which + and * are infix rather than prefix operators. Since the differentiation program is defined in terms of abstract data, we can modify it to work with different representations of expressions solely by changing the predicates, selectors, and constructors that define the representation of the algebraic expressions on which the differentiator is to operate.

a. Show how to do this in order to differentiate algebraic expressions presented in infix form, such as `(x + (3 * (x + (y + 2))))`. To simplify the task, assume that + and * always take two arguments and that expressions are fully parenthesized.

b. The problem becomes substantially harder if we allow standard algebraic notation, such as `(x + 3 * (x + y + 2))`, which drops unnecessary parentheses and assumes that multiplication is done before addition. Can you design appropriate predicates, selectors, and constructors for this notation such that our derivative program still works?

## Exercise 2.59:

Implement the `union-set` operation for the unordered-list representation of sets.

## Exercise 2.60:

We specified that a set would be represented as a list with no duplicates. Now suppose we allow duplicates. For instance, the set `{1, 2, 3}` could be represented as the list `(2 3 2 1 3 2 2)`. Design procedures `element-of-set?`, `adjoin-set`, `union-set`, and `intersection-set` that operate on this representation. How does the efficiency of each compare with the corresponding procedure for the non-duplicate representation? Are there applications for which you would use this representation in preference to the non-duplicate one?

## Exercise 2.61:

Give an implementation of `adjoin-set` using the ordered representation. By analogy with `element-of-set?` show how to take advantage of the ordering to produce a procedure that requires on the average about half as many steps as with the unordered representation.

## Exercise 2.62:

Give a Θ(n) implementation of `union-set` for sets represented as ordered lists.

## Exercise 2.63:

Each of the following two procedures converts a binary tree to a list.

```scheme
(define (tree->list-1 tree)
    (if (null? tree)
        '()
        (append (tree->list-1 (left-branch tree))
                (cons (entry tree)
                      (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
    (define (copy-to-list tree result-list)
        (if (null? tree)
            result-list
            (copy-to-list (left-branch tree)
                (cons (entry tree)
                      (copy-to-list (right-branch tree) result-list)))))
    (copy-to-list tree '()))
```

a. Do the two procedures produce the same result for every tree? If not, how do the results differ? What lists do the two procedures produce for the trees in Figure 2.16?

b. Do the two procedures have the same order of growth in the number of steps required to convert a balanced tree with n elements to a list? If not, which one grows more slowly?

## Exercise 2.64:

The following procedure `list->tree` converts an ordered list to a balanced binary tree. The helper procedure `partial-tree` takes as arguments an integer *n* and list of at least *n* elements and constructs a balanced tree containing the first *n* elements of the list. The result returned by `partial-tree` is a pair (formed with `cons`) whose `car` is the constructed tree and whose `cdr` is the list of elements not included in the tree.

a. Write a short paragraph explaining as clearly as you can how `partial-tree` works. Draw the tree produced by `list->tree` for the list `(1 3 5 7 9 11)`.

b. What is the order of growth in the number of steps required by `list->tree` to convert a list of *n* elements?

## Exercise 2.65:

Use the results of Exercise 2.63 and Exercise 2.64 to give Θ(n) implementations of `union-set` and
`intersection-set` for sets implemented as (balanced) binary trees.

## Exercise 2.66:

Implement the `lookup` procedure for the case where the set of records is structured as a binary tree, ordered by the numerical values of the keys.

## Exercise 2.67:

Define an encoding tree and a sample message:

```scheme
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
```

Use the `decode` procedure to decode the message, and give the result.

## Exercise 2.68:

The `encode` procedure takes as arguments a message and a tree and produces the list of bits that gives the encoded message.

```scheme
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
```

`encode-symbol `is a procedure, which you must write, that returns the list of bits that encodes a given symbol accord- ing to a given tree. You should design `encode-symbol` so that it signals an error if the symbol is not in the tree at all. Test your procedure by encoding the result you obtained in Exercise 2.67 with the sample tree and seeing whether it is the same as the original sample message.

## Exercise 2.69:

The following procedure takes as its argument a list of symbol-frequency pairs (where no symbol appears in more than one pair) and generates a Huffman encoding tree according to the Huffman algorithm.

```scheme
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
```

`make-leaf-set` is the procedure given above that transforms the list of pairs into an ordered set of leaves. `successive-merge` is the procedure you must write, using `make-code-tree` to successively merge the smallest-weight elements of the set until there is only one element left, which is the desired Huffman tree. (This procedure is slightly tricky, but not really complicated. If you find yourself designing a complex procedure, then you are almost certainly doing something wrong. You can take significant advantage of the fact that we are using an ordered set representation.)

## Exercise 2.70:

The following eight-symbol alphabet with associated relative frequencies was designed to efficiently encode the lyrics of 1950s rock songs. (Note that the “symbols” of an “alphabet” need not be individual letters.)

```
A 2    GET 2 SHA 3 WAH 1
BOOM 1 JOB 2 NA 16 YIP 9
```

Use `generate-huffman-tree` (Exercise 2.69) to generate a corresponding Huffman tree, and use `encode` (Exercise 2.68) to encode the following message:

```
Get a job
Sha na na na na na na na na
Get a job
Sha na na na na na na na na
Wah yip yip yip yip yip yip yip yip yip
Sha boom
```

How many bits are required for the encoding? What is the smallest number of bits that would be needed to encode this song if we used a fixed-length code for the eight-symbol alphabet?

## Exercise 2.71:

Suppose we have a Huffman tree for an alphabet of *n* symbols, and that the relative frequencies of the symbols are 1, 2, 4, . . . , 2<sup>n-1</sup>. Sketch the tree for *n* = 5; for *n* = 10. In such a tree (for general *n*) how many bits are required to encode the most frequent symbol? The least frequent symbol?

## Exercise 2.72:

Consider the encoding procedure that you designed in Exercise 2.68. What is the order of growth in the number of steps needed to encode a symbol? Be sure to include the number of steps needed to search the symbol list at each node encountered. To answer this question in general is difficult. Consider the special case where the relative frequencies of the *n* symbols are as described in Exercise 2.71, and give the order of growth (as a function of *n*) of the number of steps needed to encode the most frequent and least frequent symbols in the alphabet.

The most frequent: n(search list) + 1(encode-symbol-1). Order of growth is Θ(n)

The least frequent: ((n) + (n - 1) + (n - 2) + ... + (1)) + n(encode-symbol-1) = n(n+1)/2 + n.
Order of growth is Θ(n^2)
