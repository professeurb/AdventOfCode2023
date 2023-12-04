# [Day 4](https://adventofcode.com/2023/day/4): Scratchcards

A nice and rather easy problem, with no parsing difficulties.

Of course, one should avoid recursion at all cost.

## Python

In `adc04.py`, we provide a not very efficient solution (in particular, the computation of the score has a very bad complexity, but here, we only have small entries).

In `adc04b.py`, we improve the efficiency of the computation of the score using sets. Moreover, the use of dictionnaries (and even _defaultdicts_ here), we can process everything with a single loop.

## OCaml

This version resembles the first python version, with the use of arrays in the second part.
