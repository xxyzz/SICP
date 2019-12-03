# 1.3 Formulating Abstractions with Higher-Order Procedures

## Exercise 1.29:

Simpson’s Rule is a more accurate method of numerical integration than the method illustrated above. Using Simpson’s Rule, the integral of a function f between a and b is approximated as

h / 3 * (y<sub>0</sub> + 4y<sub>1</sub> + 2y<sub>2</sub> + 4y<sub>3</sub> + 2y<sub>4</sub> + ... + 4y<sub>n-1</sub> + y<sub>n</sub>),

where h = (b − a)/n, for some even integer n, and y<sub>k</sub> = f (a + kh). (Increasing n increases the accuracy of the approximation.) Define a procedure that takes as arguments f, a, b, and n and returns the value of the integral, computed using Simpson’s Rule. Use your procedure to integrate `cube` between 0 and 1 (with n = 100 and n = 1000), and compare the results to those of the `integral` procedure shown above.
