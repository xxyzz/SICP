# 1.3 Formulating Abstractions with Higher-Order Procedures

## Exercise 1.29:

Simpson’s Rule is a more accurate method of numerical integration than the method illustrated above. Using Simpson’s Rule, the integral of a function f between a and b is approximated as

h / 3 * (y<sub>0</sub> + 4y<sub>1</sub> + 2y<sub>2</sub> + 4y<sub>3</sub> + 2y<sub>4</sub> + ... + 4y<sub>n-1</sub> + y<sub>n</sub>),

where h = (b − a)/n, for some even integer n, and y<sub>k</sub> = f (a + kh). (Increasing n increases the accuracy of the approximation.) Define a procedure that takes as arguments f, a, b, and n and returns the value of the integral, computed using Simpson’s Rule. Use your procedure to integrate `cube` between 0 and 1 (with n = 100 and n = 1000), and compare the results to those of the `integral` procedure shown above.

## Exercise 1.30:

The `sum` procedure above generates a linear recursion. The procedure can be rewritten so that the sum is performed iteratively. Show how to do this by filling in the missing expressions in the following definition:

```scheme
(define (sum term a next b)
    (define (iter a result)
        (if ⟨??⟩
            ⟨??⟩
            (iter ⟨??⟩ ⟨??⟩)))
        (iter ⟨??⟩ ⟨??⟩))
```

## Exercise 1.31:

a. The `sum` procedure is only the simplest of a vast number of similar abstractions that can be captured as higher-order procedures. Write an analogous procedure called `product` that returns the product of the values of a function at points over a given range. Show how to define `factorial` in terms of `product`. Also use `product` to compute approximations to *π* using the formula

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\pi}{4}=\frac{2&space;\cdot&space;4&space;\cdot&space;4&space;\cdot&space;6&space;\cdot&space;6&space;\cdot&space;8&space;\cdot&space;\cdot&space;\cdot&space;}{3&space;\cdot&space;3&space;\cdot&space;5&space;\cdot&space;5&space;\cdot&space;7&space;\cdot&space;7&space;\cdot&space;\cdot&space;\cdot&space;}." target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\pi}{4}=\frac{2&space;\cdot&space;4&space;\cdot&space;4&space;\cdot&space;6&space;\cdot&space;6&space;\cdot&space;8&space;\cdot&space;\cdot&space;\cdot&space;}{3&space;\cdot&space;3&space;\cdot&space;5&space;\cdot&space;5&space;\cdot&space;7&space;\cdot&space;7&space;\cdot&space;\cdot&space;\cdot&space;}." title="\frac{\pi}{4}=\frac{2 \cdot 4 \cdot 4 \cdot 6 \cdot 6 \cdot 8 \cdot \cdot \cdot }{3 \cdot 3 \cdot 5 \cdot 5 \cdot 7 \cdot 7 \cdot \cdot \cdot }." /></a>

b. If your `product` procedure generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.

## Exercise 1.32:

a. Show that `sum` and `product` (Exercise 1.31) are both special cases of a still more general notion called `accumulate` that combines a collection of terms, using some general accumulation function:

```scheme
(accumulate combiner null-value term a next b)
```

`accumulate` takes as arguments the same term and range specifications as `sum` and `product`, together with
a `combiner` procedure (of two arguments) that specifies how the current term is to be combined with the accumulation of the preceding terms and a `null-value` that specifies what base value to use when the terms
run out. Write `accumulate` and show how `sum` and `product` can both be defined as simple calls to `accumulate`.

b. If your `accumulate` procedure generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.

## Exercise 1.33:

You can obtain an even more general version of `accumulate` (Exercise 1.32) by introducing the notion of a *filter* on the terms to be combined. That is, combine only those terms derived from values in the range that satisfy a specified condition. The resulting `filtered-accumulate` abstraction takes the same arguments as accumulate, together with an additional predicate of one argument that specifies the filter. Write `filtered-accumulate` as a procedure. Show how to express the following using `filtered-accumulate`:

a. the sum of the squares of the prime numbers in the interval *a* to *b* (assuming that you have a `prime?` predicate already written)

b. the product of all the positive integers less than *n* that are relatively prime to *n* (i.e., all positive integers *i* < *n* such that `GCD(i,n) = 1`).

## Exercise 1.34:

Suppose we define the procedure

```scheme
(define (f g) (g 2))
```

Then we have

```scheme
(f square)
; 4
(f (lambda (z) (* z (+ z 1)))) 
; 6
```

What happens if we (perversely) ask the interpreter to evaluate the combination `(f f)`? Explain.

You will get not this error:

```
application: not a procedure;
 expected a procedure that can be applied to arguments
  given: 2
  arguments...:
```

`g` needs to be a procedure which can take `2` as argument. But you can't put a `2` in `f`, because `f` needs a procedure.

## Exercise 1.35:

Show that the golden ratio *φ* (Section 1.2.2) is a fixed point of the transformation *x* → 1 + 1/*x*, and use this fact to compute *φ* by means of the `fixed-point` procedure.

## Exercise 1.36:

Modify `fixed-point` so that it prints the sequence of approximations it generates, using the `newline` and `display` primitives shown in Exercise 1.22. Then find a solution to x<sup>x</sup> = 1000 by finding a fixed point of x → log(1000)/ log(`x`). (Use Scheme’s primitive `log` procedure, which computes natural logarithms.) Compare the number of steps this takes with and without average damping. (Note that you cannot start `fixed-point` with a guess of 1, as this would cause division by log(1) = 0.)

## Exercise 1.37:

a. An infinite *continued* fraction is an expression of the form

<a href="https://www.codecogs.com/eqnedit.php?latex=\dpi{200}&space;f&space;=&space;\frac{N_{1}}{D_{1}&space;&plus;&space;\frac{N_{2}}{D_{2}&space;&plus;&space;\frac{N_{3}}{D_{3}&space;&plus;&space;...}}}." target="_blank"><img src="https://latex.codecogs.com/gif.latex?\dpi{200}&space;f&space;=&space;\frac{N_{1}}{D_{1}&space;&plus;&space;\frac{N_{2}}{D_{2}&space;&plus;&space;\frac{N_{3}}{D_{3}&space;&plus;&space;...}}}." title="f = \frac{N_{1}}{D_{1} + \frac{N_{2}}{D_{2} + \frac{N_{3}}{D_{3} + ...}}}." /></a>

As an example, one can show that the infinite continued fraction expansion with the N<sub>i</sub> and the D<sub>i</sub> all equal to 1 produces 1/φ, where φ is the golden ratio (described in Section 1.2.2). One way to approximate an infinite continued fraction is to truncate the expansion after a given number of terms. Such a truncation--a so-called *k-term finite continued fraction*—-has the form

<a href="https://www.codecogs.com/eqnedit.php?latex=\dpi{200}&space;\frac{N_{1}}{D_{1}&space;&plus;&space;\frac{N_{2}}{...&space;&plus;&space;\frac{N_{k}}{D_{k}&space;&plus;&space;...}}}." target="_blank"><img src="https://latex.codecogs.com/gif.latex?\dpi{200}&space;\frac{N_{1}}{D_{1}&space;&plus;&space;\frac{N_{2}}{...&space;&plus;&space;\frac{N_{k}}{D_{k}&space;&plus;&space;...}}}." title="\frac{N_{1}}{D_{1} + \frac{N_{2}}{... + \frac{N_{k}}{D_{k} + ...}}}." /></a>

Suppose that `n` and `d` are procedures of one argument (the term index *i*) that return the N<sub>i</sub> and D<sub>i</sub> of the terms of the continued fraction. Define a procedure `cont-frac` such that evaluating `(cont-frac n d k)` computes the value of the *k*-term finite continued fraction. Check your procedure by approximating 1/φ using

```scheme
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           k)
```

for successive values of `k`. How large must you make `k` in order to get an approximation that is accurate to 4 decimal places?

b. If your `cont-frac` procedure generates are cursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.

