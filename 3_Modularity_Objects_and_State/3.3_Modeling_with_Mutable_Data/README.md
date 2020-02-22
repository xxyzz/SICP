# 3.3 Modeling with Mutable Data

## Exercise 3.12:

Page 345

## Exercise 3.13:

Consider the following `make-cycle` procedure, which uses the `last-pair` procedure defined in Exercise 3.12:

```scheme
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
```

Draw a box-and-pointer diagram that shows the structure `z` created by

```scheme
(define z (make-cycle (list 'a 'b 'c)))
```

What happens if we try to compute `(last-pair z)`?

## Exercise 3.14:

The following procedure is quite useful, although obscure:

```scheme
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
```

`loop` uses the “temporary” variable `temp` to hold the old value of the `cdr` of `x`, since the `set-cdr!` on the next line destroys the `cdr`. Explain what `mystery` does in general. Suppose `v` is defined by `(define v (list 'a 'b 'c 'd))`. Draw the box-and-pointer diagram that represents the list to which `v` is bound. Suppose that we now evaluate `(define w (mystery v))`. Draw box-and-pointer diagrams that show the structures `v` and `w` after evaluating this expression. What would be printed as the values of `v` and `w`?

## Exercise 3.15:

Draw box-and-pointer diagrams to explain the effect of `set-to-wow!` on the structures `z1` and `z2` above.

```
🛑 is null

z1->⬛ ⬛
    |  |
 x->⬛ ⬛->⬛ 🛑
    |      |
   wow     b

z2->⬛ ⬛->⬛ ⬛->⬛ 🛑
    |      |       |
    |     wow      b
    |              |
    ------>⬛ ⬛->⬛ 🛑
           |
          wow
```

## Exercise 3.16:

Ben Bitdiddle decides to write a procedure to count the number of pairs in any list structure. “It’s easy,” he reasons. “The number of pairs in any structure is the number in the `car` plus the number in the `cdr` plus one more to count the current pair.” So Ben writes the following procedure:

```scheme
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
      1)))
```

Show that this procedure is not correct. In particular, draw box-and-pointer diagrams representing list structures made up of exactly three pairs for which Ben’s procedure would return 3; return 4; return 7; never return at all.

## Exercise 3.17:

Devise a correct version of the `count-pairs` procedure of Exercise 3.16 that returns the number of distinct pairs in any structure. (Hint: Traverse the structure, maintaining an auxiliary data structure that is used to keep track of which pairs have already been counted.)

## Exercise 3.18:

Write a procedure that examines a list and determines whether it contains a cycle, that is, whether a program that tried to find the end of the list by taking successive `cdrs` would go into an infinite loop. Exercise 3.13 constructed such lists.

## Exercise 3.19:

Redo Exercise 3.18 using an algorithm that takes only a constant amount of space. (This requires a very clever idea.)

## Exercise 3.20:

Draw environment diagrams to illustrate the evaluation of the sequence of expressions

```scheme
(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)
(car x)
; 17
```

using the procedural implementation of pairs given above. (Compare Exercise 3.11.)
