# [Day 23](https://adventofcode.com/2023/day/23): A Long Walk

A nice problem of graph traversal, where you have to maximize the length of a path instead of minimizing it.

In the first part, a simple DFS is sufficient. But in the second part, it is too inefficient. But if you prune the graph, you go from 9000 nodes down to 36. Much better, and a simple DFS does the trick in 8 sec.

A second version (where hashtables are replaced by arrays), you get another x10 speedup.
