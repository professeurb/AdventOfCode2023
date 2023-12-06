# [Day 6](https://adventofcode.com/2023/day/6): Wait For It

Again, a nice and rather easy problem, with no parsing difficulties.

However, many methods are possible, and we propose **3** solutions, all written in OCaml.

## Method 1, brute force

We have some millions of iterations to do, nothing scary for a modern computer. Quite instantaneous.

## Methode 2, math!

Basically, we have to solve a quadratic equation. For those who have not forgotten everything, this is quite easy.

But... basically, you have to use float, which is not totally satisfactory (it is possible to stick to integers by programming an efficient integer square root operation, but one has to be very careful).

## Methode 3, bisection

Another efficient method for the more algorithmically inclided relies on the bisection algorithm. The travelled distances are symmetric w.r.t. half the total time, so that one has to find between 0 and half the total time when you press the button long enough to beat the record. Same on the second half.
