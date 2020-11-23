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
 |    v   v           v  v   |      ---
 |   -------        -------  |     | n |
 |   \  *  /        \  +  /  |      ---
 |    -----          -----   |
 |      |              |     |
 --------              -------

;; controller:

    start
      |
      v
 --------------
| product <- 1 |
 --------------
      |
      v
 --------------
| counter <- 1 |
 --------------
      |
      v
     /\
    /  \  no   ------------------------------
-->/    \---> | product <- product * counter |
|  \  > /      ------------------------------
|   \  /                   |
|    \/                    v
|    | yes     ------------------------------
|    v        | counter <- counter + 1       |
|   done       ------------------------------
|                          v
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

### Exercise 5.5:

Hand-simulate the factorial and Fibonacci machines, using some nontrivial input (requiring execution of at least one recursive call). Show the contents of the stack at each significant point in the execution.

3!:

```
(controller
    (assign continue (label fact-done)) ;set up final return address  L1
  fact-loop
    (test (op =) (reg n) (const 1))
    (branch (label base-case))
    ;; Set up for the recursive call by saving n and continue.
    ;; Set up continue so that the computation will continue
    ;; at after-fact when the subroutine returns.
    (save continue)                                                   L2
    (save n)                                                          L3
    (assign n (op -) (reg n) (const 1))                               L4
    (assign continue (label after-fact))                              L5
    (goto (label fact-loop))
  after-fact
    (restore n)                                                       L6
    (restore continue)                                                L7
    (assign val (op *) (reg n) (reg val)) ;val now contains n(n - 1)! L8
    (goto (reg continue))                 ;return to caller
  base-case
    (assign val (const 1))                ;base case: 1! = 1          L9
    (goto (reg continue))                 ;return to caller
  fact-done)
```

| stack continue        | stack n | reg continue | reg n | value | line |
|-----------------------|---------|--------------|-------|-------|------|
|                       |         | fact-done    |     3 |       | L1   |
| fact-done             |         | fact-done    |     3 |       | L2   |
| fact-done             |       3 | fact-done    |     3 |       | L3   |
| fact-done             |       3 | fact-done    |     2 |       | L4   |
| fact-done             |       3 | after-fact   |     2 |       | L5   |
| after-fact->fact-done |       3 | after-fact   |     2 |       | L2   |
| after-fact->fact-done |    2->3 | after-fact   |     2 |       | L3   |
| after-fact->fact-done |    2->3 | after-fact   |     1 |       | L4   |
| after-fact->fact-done |    2->3 | after-fact   |     1 |       | L5   |
| after-fact->fact-done |    2->3 | after-fact   |     1 |     1 | L9   |
| after-fact->fact-done |       3 | after-fact   |     2 |     1 | L6   |
| fact-done             |       3 | after-fact   |     2 |     1 | L7   |
| fact-done             |       3 | after-fact   |     2 |     2 | L8   |
| fact-done             |         | after-fact   |     3 |     2 | L6   |
|                       |         | fact-done    |     3 |     2 | L7   |
|                       |         | fact-done    |     3 |     6 | L8   |

fib 3:

```
(controller
    (assign continue (label fib-done))                               L1
  fib-loop
    (test (op <) (reg n) (const 2))
    (branch (label immediate-answer))
    ;; set up to compute Fib(n − 1)
    (save continue)                                                  L2
    (assign continue (label afterfib-n-1))                           L3
    (save n)                 ; save old value of n                   L4
    (assign n (op -) (reg n) (const 1)) ; clobber n to n-1           L5
    (goto (label fib-loop))  ; perform recursive call
  afterfib-n-1     ; upon return, val contains Fib(n − 1)
    (restore n)                                                      L6
    (restore continue)                                               L7
    ;; set up to compute Fib(n − 2)
    (assign n (op -) (reg n) (const 2))                              L8
    (save continue)                                                  L9
    (assign continue (label afterfib-n-2))                           L10
    (save val)               ; save Fib(n − 1)                       L11
    (goto (label fib-loop))
  afterfib-n-2     ; upon return, val contains Fib(n − 2)
    (assign n (reg val))       ; n now contains Fib(n − 2)           L12
    (restore val)              ; val now contains Fib(n − 1)         L13
    (restore continue)                                               L14
    (assign val                ; Fib(n − 1) + Fib(n − 2)             L15
            (op +) (reg val) (reg n))
    (goto (reg continue))      ; return to caller, answer is in val
  immediate-answer
    (assign val (reg n))       ; base case: Fib(n) = n               L16
    (goto (reg continue))
  fib-done)
```

| stack continue         | stack n | stack val | reg continue | reg n | reg val | line |
|------------------------|---------|-----------|--------------|-------|---------|------|
|                        |         |           | fib-done     |     3 |         | L1   |
| fib-done               |         |           | fib-done     |     3 |         | L2   |
| fib-done               |         |           | afterfib-n-1 |     3 |         | L3   |
| fib-done               |       3 |           | afterfib-n-1 |     3 |         | L4   |
| fib-done               |       3 |           | afterfib-n-1 |     2 |         | L5   |
| afterfib-n-1->fib-done |       3 |           | afterfib-n-1 |     2 |         | L2   |
| afterfib-n-1->fib-done |       3 |           | afterfib-n-1 |     2 |         | L3   |
| afterfib-n-1->fib-done |    2->3 |           | afterfib-n-1 |     2 |         | L4   |
| afterfib-n-1->fib-done |    2->3 |           | afterfib-n-1 |     1 |         | L5   |
| afterfib-n-1->fib-done |    2->3 |           | afterfib-n-1 |     1 |       1 | L16  |
| afterfib-n-1->fib-done |       3 |           | afterfib-n-1 |     2 |       1 | L6   |
| fib-done               |       3 |           | afterfib-n-1 |     2 |       1 | L7   |
| fib-done               |       3 |           | afterfib-n-1 |     0 |       1 | L8   |
| afterfib-n-1->fib-done |       3 |           | afterfib-n-1 |     0 |       1 | L9   |
| afterfib-n-1->fib-done |       3 |           | afterfib-n-2 |     0 |       1 | L10  |
| afterfib-n-1->fib-done |       3 |         1 | afterfib-n-2 |     0 |       1 | L11  |
| afterfib-n-1->fib-done |       3 |         1 | afterfib-n-2 |     0 |       0 | L16  |
| afterfib-n-1->fib-done |       3 |         1 | afterfib-n-2 |     0 |       0 | L12  |
| afterfib-n-1->fib-done |       3 |           | afterfib-n-2 |     0 |       1 | L13  |
| fib-done               |       3 |         1 | afterfib-n-1 |     0 |       1 | L14  |
| fib-done               |       3 |         1 | afterfib-n-1 |     0 |       1 | L15  |
| fib-done               |         |         1 | afterfib-n-1 |     3 |       1 | L6   |
|                        |         |         1 | fib-done     |     3 |       1 | L7   |
|                        |         |         1 | fib-done     |     1 |       1 | L8   |
| fib-done               |         |         1 | fib-done     |     1 |       1 | L9   |
| fib-done               |         |         1 | afterfib-n-2 |     1 |       1 | L10  |
| fib-done               |         |      1->1 | afterfib-n-2 |     1 |       1 | L11  |
| fib-done               |         |      1->1 | afterfib-n-2 |     1 |       1 | L16  |
| fib-done               |         |      1->1 | afterfib-n-2 |     1 |       1 | L12  |
| fib-done               |         |         1 | afterfib-n-2 |     1 |       1 | L13  |
|                        |         |         1 | fib-done     |     1 |       1 | L14  |
|                        |         |         1 | fib-done     |     1 |       2 | L15  |

### Exercise 5.6:

Ben Bitdiddle observes that the Fibonacci machine’s controller sequence has an extra `save` and an extra `restore`, which can be removed to make a faster machine. Where are these instructions?

`save` at L9, `restore` at L7.

## 5.1.5 Instruction Summary
