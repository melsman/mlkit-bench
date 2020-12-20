## Parallel Benchmarks

Here is the output from running `make press` (headers slightly
modified) on a MacBook Pro (15-inch, 2016) with a 2,7 GHz Quad-Core
Intel Core i7 CPU:

```
 |          | MLton 20201023 | MLKitSeq      | MLKitPar - 1 Core | MLKitPar - 4 Cores |
 | Program  | real time (s)  | real time (s) | real time (s)     | real time (s)      |
 +----------+----------------+---------------+-------------------+--------------------+
 | fib      | 0.51 ±2.2%     | 0.52 ±1.5%    | 0.55 ±1.3%        | 0.10 ±5.0%         |
 | nqueens  | 1.26 ±0.9%     | 1.63 ±2.0%    | 1.66 ±0.5%        | 0.91 ±0.9%         |
 | pmsort   | 0.40 ±2.3%     | 0.19 ±0.0%    | 0.45 ±1.1%        | 0.24 ±2.1%         |
 | ray-orig | 3.86 ±0.8%     | 7.75 ±1.1%    | 16.57 ±0.3%       | 4.41 ±1.4%         |
 | ray      | 3.88 ±0.7%     | 2.28 ±1.0%    | 2.37 ±1.2%        | 0.53 ±0.0%         |
```

We need to investigate the overhead of the parallel runtime and the
parallel generated code for the `pmsort` benchmark.

Please notice that some of the benchmarks (especially the `ray` benchmark)
have been hand-optimised for running well with MLKit (`ray-orig` is
Troels Henriksen's original version). In particular, for the `ray` benchmark,
special blocks of unboxed reals are used for avoiding the construction
of tuples of boxed reals. We see that MLton's whole-program
optimisation techniques do not result in different execution times for
the modified and the original version of the `ray` benchmark.
