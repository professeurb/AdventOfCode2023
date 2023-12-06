# [Day 5](https://adventofcode.com/2023/day/5): If You Give A Seed A Fertilizer

Today, we attempt to compose transformation, efficiently if possible.

## OCaml

A rather naive approach is perfectly fine for the first part, as implemented in `adc05.ml`. But for this second part, it is more problematic (even though it takes 10 minutes on my laptop).

We have to be more subtle. In `adc05b`, we consider lists of ordered intervals. This is extremely fast, but the different cases for the possible relative position of two intervals has to be done carefully. Now, we are down to 70ms.

## Python

Not today.
