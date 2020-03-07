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

## Exercise 3.21:

Page 359

## Exercise 3.22:

Instead of representing a queue as a pair of pointers, we can build a queue as a procedure with local state. The local state will consist of pointers to the beginning and the end of an ordinary list. Thus, the `make-queue` procedure will have the form

```scheme
(define (make-queue)
  (let ((front-ptr . . . )
        (rear-ptr . . . ))
    ⟨definitions of internal procedures⟩
    (define (dispatch m) . . .)
    dispatch))
```

Complete the definition of `make-queue` and provide implementations of the queue operations using this representation.

## Exercise 3.23:

A *deque* (“double-ended queue”) is a sequence in which items can be inserted and deleted at either the front or the rear. Operations on deques are the constructor `make-deque`, the predicate `empty-deque?`, selectors `front-deque` and `rear-deque`, mutators `front-insert-deque!`, `rear-insert-deque!`, `front-delete-deque!`, and `rear-delete-deque!`. Show how to represent deques using pairs, and give implementations of the operations. should be accomplished in Θ(1) steps.

## Exercise 3.24:

In the table implementations above, the keys are tested for equality using `equal?` (called by `assoc`). This is not always the appropriate test. For instance, we might have a table with numeric keys in which we don’t need an exact match to the number we’re looking up, but only a number within some tolerance of it. Design a table constructor `make-table` that takes as an argument a `same-key?` procedure that will be used to test “equality” of keys. `make-table` should return a dispatch procedure that can be used to access appropriate `lookup` and `insert!` procedures for a local table.

## Exercise 3.25:

Generalizing one- and two-dimensional tables, show how to implement a table in which values are stored under an arbitrary number of keys and different values may be stored under different numbers of keys. The `lookup` and `insert!` procedures should take as input a list of keys used to access the table.

## Exercise 3.26:

To search a table as implemented above, one needs to scan through the list of records. This is basically the unordered list representation of Section 2.3.3. For large tables, it may be more efficient to structure the table in a different manner. Describe a table implementation where the (key, value) records are organized using a binary tree, assuming that keys can be ordered in some way (e.g., numerically or alphabetically). (Compare Exercise 2.66 of Chapter 2.)

## Exercise 3.27

Page 368

It will only memoize the result.

## Exercise 3.28:

Define an `or-gate` as a primitive function box. Your `or-gate` constructor should be similar to `and-gate`.

## Exercise 3.29:

Another way to construct an `or-gate` is as a compound digital logic device, built from `and-gates` and `inverters`. Define a procedure `or-gate` that accomplishes this. What is the delay time of the `or-gate` in terms of `and-gate-delay` and `inverter-delay`?

## Exercise 3.30:

Page 376

## Exercise 3.31:

The internal procedure `accept-action-procedure!` defined in `make-wire` specifies that when a new action procedure is added to a wire, the procedure is immediately run. Explain why this initialization is necessary. In particular, trace through the half-adder example in the paragraphs above and say how the system’s response would differ if we had defined `accept-action-procedure!` as

```scheme
(define (accept-action-procedure! proc)
  (set! action-procedures
        (cons proc action-procedures)))
```

## Exercise 3.32:

The procedures to be run during each time segment of the agenda are kept in a queue. Thus, the procedures for each segment are called in the order in which they were added to the agenda (first in, first out). Explain why this order must be used. In particular, trace the behavior of an and-gate whose inputs change from 0, 1 to 1, 0 in the same segment and say how the behavior would differ if we stored a segment’s procedures in an ordinary list, adding and removing procedures only at the front (last in, first out).
