# 5.3 Storage Allocation and Garbage Collection

## 5.3.1 Memory as Vectors

### Exercise 5.20:

Draw the box-and-pointer representation and the memory-vector representation (as in Figure 5.14) of the list structure produced by

```scheme
(define x (cons 1 2))
(define y (list x x))
```

with the `free` pointer initially `p1`. What is the final value of `free` ? What pointers represent the values of `x` and `y` ?

```
    --- ---    --- ---
y->| ● | ● |->| ● | / |
    --- ---    --- ---
     |          |
     |-----------
     V
    --- ---
x->| ● | ● |
    --- ---
     |   |
     V   V
    --- ---
   | 1 | 2 |
    --- ---
```

| Index    | 1  | 2  | 3  |
|----------|----|----|----|
| the-cars | n1 | p1 | p1 |
| the-cdrs | n2 | e0 | p3 |

The second pair of `y` is stored first.

### Exercise 5.21:

Implement register machines for the following procedures. Assume that the list-structure memory operations are available as machine primitives.

a. Recursive `count-leaves`:

```scheme
(define (count-leaves tree)
  (cond ((null? tree) 0)
  ((not (pair? tree)) 1)
  (else (+ (count-leaves (car tree))
           (count-leaves (cdr tree))))))
```

b. Recursive `count-leaves` with explicit counter:

```scheme
(define (count-leaves tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
    ((not (pair? tree)) (+ n 1))
    (else
      (count-iter (cdr tree)
                  (count-iter (car tree)
                              n)))))
  (count-iter tree 0))
```

### Exercise 5.22:

Exercise 3.12 of Section 3.3.1 presented an `append` procedure that appends two lists to form a new list and an `append!` procedure that splices two lists together. Design a register machine to implement each of these procedures. Assume that the list-structure memory operations are available as primitive operations.

## 5.3.2 Maintaining the Illusion of Infinite Memory
