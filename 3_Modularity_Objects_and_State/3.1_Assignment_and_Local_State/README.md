# 3.1 Assignment and Local State

## Exercise 3.1:

An *accumulator* is a procedure that is called repeatedly with a single numeric argument and accumulates its arguments into a sum. Each time it is called, it returns the currently accumulated sum. Write a procedure `make-accumulator` that generates accumulators, each maintaining an independent sum. The input to `make-accumulator` should specify the initial value of the sum; for example

```scheme
(define A (make-accumulator 5))
(A 10)
; 15
(A 10)
; 25
```

## Exercise 3.2:

In software-testing applications, it is useful to be able to count the number of times a given procedure is called during the course of a computation. Write a procedure `make-monitored` that takes as input a procedure, `f`, that itself takes one input. The result returned by `make-monitored` is a third procedure, say `mf`, that keeps track of the number of times it has been called by maintaining an internal counter. If the input to `mf` is the special symbol `how-many-calls?`, then `mf` returns the value of the counter. If the input is the special symbol `reset-count`, then `mf` resets the counter to zero. For any other input, `mf` returns the result of calling `f` on that input and increments the counter. For instance, we could make a monitored version of the `sqrt` procedure:

```scheme
(define s (make-monitored sqrt))
(s 100)
; 10
(s 'how-many-calls?)
; 1
```

## Exercise 3.3:

Modify the `make-account` procedure so that it creates `password-protected` accounts. That is, `make-account` should take a symbol as an additional argument, as in

```scheme
(define acc (make-account 100 'secret-password))
```

The resulting account object should process a request only if it is accompanied by the password with which the account was created, and should otherwise return a complaint:

```scheme
((acc 'secret-password 'withdraw) 40)
; 60
((acc 'some-other-password 'deposit) 50)
; "Incorrect password"
```

## Exercise 3.4:

Modify the `make-account` procedure of Exercise 3.3 by adding another local state variable so that, if an account is accessed more than seven consecutive times with an incorrect password, it invokes the procedure `call-the-cops`.

## Exercise 3.5:

Page 309.

## Exercise 3.6:

It is useful to be able to reset a `random-number` generator to produce a sequence starting from a given value. Design a new rand procedure that is called with an argument that is either the symbol `generate` or the symbol reset and behaves as follows: `(rand 'generate)` produces a new random number; `((rand 'reset) ⟨new-value⟩)` resets the internal state variable to the designated `⟨new-value⟩`. Thus, by resetting the state, one can generate repeatable sequences. These are very handy to have when testing and debugging programs that use random numbers.

## Exercise 3.7:

Consider the bank account objects created by `make-account`, with the password modification described in Exercise 3.3. Suppose that our banking system requires the ability to make joint accounts. Define a procedure `make-joint` that accomplishes this. `make-joint` should take three arguments. The first is a password-protected account. The second argument must match the password with which the account was defined in order for the `make-joint` operation to proceed. The third argument is a new password. `make-joint` is to create an additional access to the original account using the new password. For example, if` peter-acc` is a bank account with password `open-sesame`, then

```scheme
(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))
```

will allow one to make transactions on `peter-acc` using the name `paul-acc` and the password `rosebud`. You may wish to modify your solution to Exercise 3.3 to accommodate this new feature.

## Exercise 3.8:

When we defined the evaluation model in Section 1.1.3, we said that the first step in evaluating an expression is to evaluate its subexpressions. But we never specified the order in which the subexpressions should be evaluated (e.g., left to right or right to left). When we introduce assignment, the order in which the arguments to a procedure are evaluated can make a difference to the result. Define a simple procedure `f` such that evaluating

```scheme
(+ (f 0) (f 1))
```

will return 0 if the arguments to `+` are evaluated from left to right but will return 1 if the arguments are evaluated from right to left.
