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
