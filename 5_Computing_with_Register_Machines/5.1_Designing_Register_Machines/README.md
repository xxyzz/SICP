# 5.1 Designing Register Machines

### Exercise 5.1:

Design a register machine to compute factorials using the iterative algorithm specified by the following procedure. Draw data-path and controller diagrams for this machine.

```scheme
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))
```

```
;; data-path:
                                    ---
 ---------          ---------      /   \
| product |        | counter |---->| > |
 ---------          ---------      \   /
 ^    |   ----------| |  /\  ^      ---
 |    |   |           | /1 \ |       ^
 |    |   |           | ---- |       |
 |   \|/ \|/         \|/\|/  |      ---
 |   -------        -------  |     | n |
 |   \  *  /        \  +  /  |      ---
 |    -----          -----   |
 |      |              |     |
 --------              -------

;; controller:

    start
      |
     \|/
 --------------
| product <- 1 |
 --------------
      |
     \|/
 --------------
| counter <- 1 |
 --------------
      |
     \|/
     /\
    /  \  no   ------------------------------
-->/    \---> | product <- product * counter |
|  \  > /      ------------------------------
|   \  /                   |
|    \/                   \|/
|    | yes     -----------------------------
|   \|/       | counter <- counter + 1      |
|   done       -----------------------------
|                         \|/
|--------------------------
```

## 5.1.1 A Language for Describing Register Machines

### Exercise 5.2:

Use the register-machine language to describe the iterative factorial machine of Exercise 5.1.

```
(controller
  (assign product (const 1))
  (assign counter (const 1))
  test-counter
    (test (op >) (reg counter) (reg n))
    (branch (label factorial-done))
    (assign product (op *) (reg product) (reg counter))
    (assign counter (op +) (reg counter) (const 1))
    (goto (label test-counter))
  factorial-done)
```

## 5.1.2 Abstraction in Machine Design

### Exercise 5.3:

Design a machine to compute square roots using Newton’s method, as described in Section 1.1.7:

```scheme
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
```

Begin by assuming that `good-enough?` and `improve` operations are available as primitives. Then show how to expand these in terms of arithmetic operations. Describe each version of the `sqrt` machine design by drawing a data-path diagram and writing a controller definition in the register-machine language.

```
;; primitives
(controller
  (assign guess (const 1.0))
  test-guess
    (test (op good-enough?) (reg guess))
    (branch (label sqrt-done))
    (assign guess (op improve) (reg guess))
    (goto (label test-guess))
  sqrt-done)

;; expand
(controller
  (assign guess (const 1.0))
  test-guess
    (assign t (op *) (reg guess) (reg guess))
    (assign t (op -) (reg t) (reg x))
    (assign t (op abs) (reg t))
    (test (op <) (reg t) (const 0.001))
    (branch (label sqrt-done))
    (assign t (op /) (reg x) (reg guess))
    (assign t (op +) (reg t) (reg guess))
    (assign guess (op /) (reg t) (const 2))
    (goto (label test-guess))
  sqrt-done)
```

## 5.1.3 Subroutines

## 5.1.4 Using a Stack to Implement Recursion

### Exercise 5.4:

Specify register machines that implement each of the following procedures. For each machine, write a controller instruction sequence and draw a diagram showing the data paths.

a. Recursive exponentiation:

```scheme
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))
```

b. Iterative exponentiation:

```scheme
(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
        product
        (expt-iter (- counter 1)
                   (* b product))))
  (expt-iter n 1))
```

```
;; a
(controller
    (assign continue (label expt-done))
  expt
    (test (op =) (reg n) (const 0))
    (branch (label base-case))
    (save continue)
    (assign continue (label after-expt))
    (assign n (op -) (reg n) (const 1))
    (goto (label expt))
  after-expt
    (restore continue)
    (assign val (op *) (reg b) (reg val))
    (goto (reg continue))
  base-case
    (assign val (const 1))
    (goto (reg continue))
  expt-done)
  
;; b
(controller
    (assign product (const 1))
  expt
    (test (op =) (reg n) (const 0))
    (branch (label expt-done))
    (assign n (op -) (reg n) (const 1))
    (assign product (op *) (reg b) (reg product))
    (goto (label expt))
  expt-done)
```
