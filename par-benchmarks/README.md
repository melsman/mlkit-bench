## Parallel Benchmarks

Here is the output from running `make press` (headers slightly
modified) on a MacBook Pro (15-inch, 2016) with a 2,7 GHz Quad-Core
Intel Core i7 CPU:

```
 |         | MLKitSeq      | MLKitPar - 1 Core | MLKitPar - 4 Cores |
 | Program | real time (s) | real time (s)     | real time (s)      |
 +---------+---------------+-------------------+--------------------+
 | fib     | 0.55 ±1.9%    | 0.56 ±1.8%        | 0.11 ±4.7%         |
 | nqueens | 1.69 ±1.5%    | 1.77 ±1.5%        | 0.90 ±1.3%         |
 | pmsort  | 0.19 ±5.2%    | 0.46 ±1.1%        | 0.24 ±3.9%         |
 | ray     | 2.42 ±1.2%    | 2.51 ±1.9%        | 0.53 ±0.9%         |
```

We need to investigate the overhead of the parallel runtime and the
parallel generated code for the `pmsort` benchmark.
