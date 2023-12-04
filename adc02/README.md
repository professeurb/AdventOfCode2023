## [Day 2](https://adventofcode.com/2023/day/2): Cube Conundrum

### Python

The use of string splitting w.r.t. another string, together with dictionnaries, leads to a rather straightforward program.

### OCaml

Here, the situation is more complicated, since the splitting function for strings is not as versatile as in Python. This leads to a very complicated and unefficient program.

However, if you use a decent parser library (as I have done with [Angstrom](https://github.com/inhabitedtype/angstrom)), things become much simpler as can be seen in the second version, where the parsing of the input file is no longer the hardest part to write.
