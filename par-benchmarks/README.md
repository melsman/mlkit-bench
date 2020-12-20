## Parallel Benchmarks

Here is the output from running `make press` (headers slightly
modified) on a MacBook Pro (15-inch, 2016) with a 2,7 GHz Quad-Core
Intel Core i7 CPU:

```
 |         | MLton 20201023 | MLKitSeq      | MLKitPar - 1 Core | MLKitPar - 4 Cores |
 | Program | real time (s)  | real time (s) | real time (s)     | real time (s)      |
 +---------+----------------+---------------+-------------------+--------------------+
 | fib     | 0.53 ±2.6%     | 0.54 ±2.3%    | 0.55 ±2.2%        | 0.10 ±4.8%         |
 | nqueens | 1.33 ±1.7%     | 1.69 ±2.1%    | 1.77 ±1.5%        | 0.92 ±1.5%         |
 | pmsort  | 0.42 ±2.6%     | 0.19 ±2.6%    | 0.47 ±1.9%        | 0.24 ±3.0%         |
 | ray     | 4.11 ±0.7%     | 2.41 ±1.1%    | 2.51 ±1.6%        | 0.53 ±0.8%         |
```

We need to investigate the overhead of the parallel runtime and the
parallel generated code for the `pmsort` benchmark.

Please notice that the benchmarks (especially the `ray` benchmark) have
been hand-optimised for running well with MLKit. In particular, for
the `ray` benchmark, special blocks of unboxed reals are used for
avoiding the construction of tuples of boxed reals; perhaps MLton is
not good at detecting and optimising the particular patterns used.
