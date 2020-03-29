# 3.4 Concurrency: Time Is of the Essence

## Exercise 3.38:

Suppose that Peter, Paul, and Mary share a joint bank account that initially contains $100. Concurrently, Peter deposits $10, Paul withdraws $20, and Mary withdraws half the money in the account, by executing the following commands:

```scheme
Peter: (set! balance (+ balance 10))
Paul:  (set! balance (- balance 20))
Mary:  (set! balance (- balance (/ balance 2)))
```
a. List all the different possible values for balance after these three transactions have been completed, assuming that the banking system forces the three processes to run sequentially in some order.

Peter -> Paul -> Mary: (100 + 10 - 20) / 2 = 45

Peter -> Mary -> Paul: (100 + 10) / 2 - 20 = 35

Paul -> Peter -> Mary: (100 - 20 + 10) / 2 = 45

Paul -> Mary -> Peter: (100 - 20) / 2 + 10 = 50

Mary -> Peter -> Paul: 100 / 2 + 10 - 20 = 40

Mary -> Paul -> Peter: 100 / 2 - 20 + 10 = 40

b. What are some other values that could be produced if the system allows the processes to be interleaved? Draw timing diagrams like the one in Figure 3.29 to explain how these values can occur.

## Exercise 3.39:

Which of the five possibilities in the parallel execution shown above remain if we instead serialize execution as follows:

```scheme
(define x 10)
(define s (make-serializer))
(parallel-execute
  (lambda () (set! x ((s (lambda () (* x x))))))
  (s (lambda () (set! x (+ x 1)))))
```

101: P1 sets x to 100 and then P2 increments x to 101.

121: P2 increments x to 11 and then P1 sets x to x * x.

100: P1 accesses x (twice), then P2 sets x to 11, then P1 sets x.

11: P1 calculates square x, P2 calculates x plus one, P1 sets x to 100, P2 sets x to 11.

## Exercise 3.40:

Give all possible values of x that can result from executing

```scheme
(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))
```

Which of these possibilities remain if we instead use serialized procedures:

```scheme
(define x 10)
(define s (make-serializer))
(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))
```

P1 steps: accesses x twice, calculates x square, sets x.

P2 steps: accesses x three times, calculates x cubic, sets x.

10^6: P1 then P2 or P2 then P1.

10^3: P1 calculates x square, P2 calculates x cubic, P1 sets x, P2 sets x.

10^2: P1 calculates x square, P2 calculates x cubic, P2 sets x, P1 sets x.

10^4: P1 accesses x, P2 sets x to 1000, P1 calculates x(10 * 100) then sets x.

10^4: P2 accesses x twice, P1 sets x, P2 calculates x(* 10 10 100) then sets x.

10^5: P2 accesses x, P1 sets x, P2 calculates x(* 10 100 100) then sets x.

After using serialized procedures only get 10^6.