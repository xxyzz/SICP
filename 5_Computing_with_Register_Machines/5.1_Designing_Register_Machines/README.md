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
| produce <- 1 |
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
