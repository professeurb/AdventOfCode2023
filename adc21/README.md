# [Day 21](https://adventofcode.com/2023/day/21): Step Counter

A tough one. If the first part is not really problematic, the second part requires a bit of thinking.

The approach followed here relies on the fact that as the border of the map and the horizontal and vertical lines going through the center are only made of garden plots. In terms of Manhattan distance, this means that in order to minimize the distance, you only need to consider that you enter a new map either by the center of a side, or by a corner.

This leads to make 9 BFS corresponding to all the possible entry points (the center for the initial map, the center of a side or a corner).

Another point is that the maps can be divided into three categories:

- those where all the garden plots (of the right _parity_) can be reached with fewer steps than the target,
- those where no garden plots can be reached,
- those where only some but not all can be reached, on the _frontier_.

A first version, `adc21b.ml`, implements this with a rather simple way of counting.

A second version, `adc21c.ml`, improves the computation and provides a counting method which complexity is **constant** in the target number of steps.
