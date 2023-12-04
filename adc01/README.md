## [Day 1](https://adventofcode.com/2023/day/1): Trebuchet?!

### Python

A first solution where we compute a list of all the digits (in order) present in the string. We then only need to get the first one and the last one. We use dictionnaries to find the digits.

A second solution, faster, is based on regular expressions.

### OCaml

The first version uses the standard library. We search for string patterns using a set of hand-written pattern matchings.

The second version uses the experimental [`brucore`](https://github.com/professeurb/brucore) library which provides some extra functionalities (generators, dynamic arrays, etc.).

Not totally convincing, but it works.
