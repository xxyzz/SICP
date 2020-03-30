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

## Exercise 3.41:

Ben Bitdiddle worries that it would be better to implement the bank account as follows (where the commented line has been changed):

```scheme
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance)
             ((protected
               (lambda () balance)))) ; serialized
            (else
             (error "Unknown request: MAKE-ACCOUNT"
                    m))))
    dispatch))
```

because allowing unserialized access to the bank balance can result in anomalous behavior. Do you agree? Is there any scenario that demonstrates Ben’s concern?

No, unserialized access won't return the wrong balance.

## Exercise 3.42:

Ben Bitdiddle suggests that it’s a waste of time to create a new serialized procedure in response to every `withdraw` and `deposit` message. He says that `make-account` could be changed so that the calls to protected are done outside the `dispatch` procedure. That is, an account would return the same serialized procedure (which was created at the same time as the account) each time it is asked for a withdrawal procedure.

```scheme
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (let ((protected-withdraw (protected withdraw))
          (protected-deposit (protected deposit)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) protected-withdraw)
              ((eq? m 'deposit) protected-deposit)
              ((eq? m 'balance) balance)
              (else
               (error "Unknown request: MAKE-ACCOUNT"
                      m))))
      dispatch)))
```

If serializer can't interleave itself then it's safe. They are same.

## Exercise 3.43:

Suppose that the balances in three accounts start out as $10, $20, and $30, and that multiple processes run, exchanging the balances in the accounts. Argue that if the processes are run sequentially, after any number of concurrent exchanges, the account balances should be $10, $20, and $30 in some order. Draw a timing diagram like the one in Figure 3.29 to show how this condition can be violated if the exchanges are implemented using the first version of the account-exchange program in this section. On the other hand, argue that even with this `exchange` program, the sum of the balances in the accounts will be preserved. Draw a timing diagram to show how even this condition would be violated if we did not serialize the transactions on individual accounts.

```
account1    account2    account3
10          20          30
  <-- d10 -->   
  <--------- d20 ------->
+10         -10
20          10          30
+20                     -20
40          10          10      sum: 60

; not serialize
account1    account2    account3
10          20          30
  <-- d10 -->   
  <--------- d20 ------->
+10         -10
20          10          30
10+20                   30-20
30          10          10      sum: 50
```

Just move the difference won't change the sum of balance.

## Exercise 3.44:

Consider the problem of transferring an amount from one account to another. Ben Bitdiddle claims that this can be accomplished with the following procedure, even if there are multiple people concurrently transferring money among multiple accounts, using any account mechanism that serializes deposit and withdrawal transactions, for example, the version of `make-account` in the text above.

```scheme
(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))
```

Louis Reasoner claims that there is a problem here, and that we need to use a more sophisticated method, such as the one required for dealing with the exchange problem. Is Louis right? If not, what is the essential difference between the transfer problem and the exchange problem? (You should assume that the balance in `from-account` is at least amount.)

```scheme
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))
```

No. `exchange` needs both accounts' balances to calculates the difference, `transfer` doesn't.

## Exercise 3.45:

Louis Reasoner thinks our bank-account system is unnecessarily complex and error-prone now that deposits and withdrawals aren’t automatically serialized. He suggests that `make-account-and-serializer` should have exported the serializer (for use by such procedures as `serialized-exchange`) in addition to (rather than instead of) using it
to serialize accounts and deposits as `make-account` did. He proposes to redefine accounts as follows:

```scheme
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (balance-serializer withdraw))
            ((eq? m 'deposit) (balance-serializer deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))
```

Then deposits are handled as with the original `make-account`:

```scheme
(define (deposit account amount)
  ((account 'deposit) amount))
```

Explain what is wrong with Louis’s reasoning. In particular, consider what happens when `serialized-exchange` is called.

```scheme
(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))
```

That will serialize twice.

## Exercise 3.46:

Suppose that we implement `test-and-set!` using an ordinary procedure as shown in the text, without attempting to make the operation atomic. Draw a timing diagram like the one in Figure 3.29 to demonstrate how the mutex implementation can fail by allowing two processes to acquire the mutex at the same time.

```scheme
(define (test-and-set! cell)
  (if (car cell) true (begin (set-car! cell true) false)))
```

```
P1           mutex     P2
             false
access mutex           access mutex
set true               set true
return false true      return false

both process acquire the mutex
```

## Exercise 3.47:

A semaphore (of size *n*) is a generalization of a mutex. Like a mutex, a semaphore supports acquire and release operations, but it is more general in that up to *n* processes can acquire it concurrently. Additional processes that attempt to acquire the semaphore must wait for release operations. Give implementations of semaphores

a. in terms of mutexes

b. in terms of atomic `test-and-set!` operations.
