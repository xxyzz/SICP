# 3.2 The Environment Model of Evaluation

## Exercise 3.9:

In Section 1.2.1 we used the substitution model to analyze two procedures for computing factorials, a recursive version

```scheme
(define (factorial n)
  (if (= n 1) 1 (* n (factorial (- n 1)))))
```

and an iterative version

```scheme
(define (factorial n) (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter (* counter product)
               (+ counter 1)
               max-count)))
```

Show the environment structures created by evaluating `(factorial 6)` using each version of the `factorial` procedure.

```
recursive version:
               _________________________________________________________
global        |                                                         |
env        -->|                                                         |
              |_________________________________________________________|
                  /|\                     /|\                      /|\
(factorial 3)  ____|___                 ___|___                  ___|___
              |       |                |       |                |       |
         E1 ->| n: 3  |           E2 ->| n: 2  |           E3 ->| n: 1  |       
              |_______|                |_______|                |_______|
(* n (factorial (- n 1)))    (* n (factorial (- n 1)))             1
```

```
iterative version:
               _____________________________________________________________________________________
global        |                                                                                    |
env        -->|                                                                                    |
              |____________________________________________________________________________________|
(factorial 3) /|\                /|\                   /|\                 /|\               /|\
            ___|___        _______|_____         _______|______      _______|_____       _____|_____
           |       |      | product:  1 |       | product:  1 |     | product:  2 |     |product:6  |
       E1->| n: 3  |  E2->| counter:  1 |   E3->| counter:  2 | E4->| counter:  3 | E5->|counter:5  |
           |       |      | max-count:3 |       | max-count:3 |     | max-count:3 |     |max-count:3|
           |_______|      |_____________|       |_____________|     |_____________|     |___________|
   (fact-iter 1 1 n) (fact-iter (* counter product)   ...                 ...             product
                                (+ counter 1)              
                                max-count)))                     
```
