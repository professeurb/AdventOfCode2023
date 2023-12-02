# Advent of Code 2023

This repositery contains my solutions for the Advent of Code 2023.

## Day 1

### Python

A first solution where we compute a list of all the digits (in order) present in the string. We then only need to get the first one and the last one. We use dictionnaries to find the digits.

A second solution, faster, is based on regular expressions.

### OCaml

The first version uses the standard library. We search for string patterns using a set of hand-written pattern matchings.

The second version uses the experimental [`brucore`](https://github.com/professeurb/brucore) library which provides some extra functionalities (generators, dynamic arrays, etc.).

Not totally convinced, but it works.

## Day 2

### Python

The use of string splitting w.r.t. another string, together with dictionnaries, leads to a rather straightforward program.

### OCaml

Here, the situation is more complicated, since the splitting function for strings is not as versatile as in Python. This leads to a very complicated and unefficient program.

However, if you use a decent parser library (as I have done with [Angstrom](https://github.com/inhabitedtype/angstrom)), things become much simpler as can be seen in the second version, where the parsing of the input file is no longer the hardest part to write.
