## Parallel Benchmarks

Notice: See below for prerequisites to running `make press`...

Here is the output from running `make press` (headers slightly
modified) on a MacBook Pro (15-inch, 2016) with a 2,7 GHz Quad-Core
Intel Core i7 CPU:

```
 |          | MLton 20201023 | MLKitSeq      | MLKitPar - 1 Core | MLKitPar - 4 Cores |
 | Program  | real time (s)  | real time (s) | real time (s)     | real time (s)      |
 +----------+----------------+---------------+-------------------+--------------------+
 | fib      | 0.52 ±1.4%     | 0.49 ±2.1%    | 0.53 ±2.9%        | 0.12 ±0.0%         |
 | nqueens  | 1.30 ±1.5%     | 1.85 ±1.0%    | 1.91 ±2.1%        | 1.01 ±1.1%         |
 | pmsort   | 0.47 ±2.7%     | 0.20 ±0.0%    | 0.47 ±2.0%        | 0.26 ±2.5%         |
 | ray-orig | 3.95 ±0.9%     | 7.80 ±0.6%    | 16.95 ±0.8%       | 4.02 ±1.1%         |
 | ray      | 3.98 ±0.7%     | 2.39 ±0.9%    | 2.46 ±1.8%        | 0.54 ±0.0%         |
 | scan     | 0.10 ±4.8%     | 0.16 ±0.0%    | 0.16 ±0.0%        | 0.05 ±9.7%         |
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

The `scan` benchmark makes use of the `util/soac.sml` library, which
implements a series of data-parallel combinators on pull-arrays.

### Prerequisites

Running these benchmarks (using `make press`) assumes the following:

1. MLton is installed and available as a command `mlton`.
2. The `smlpkg` tool is available as a command `smlpkg`.
3. A new version of MLKit is installed. You can set the root of the installation using the `MLKIT_ROOT` environment variable (the default location is `~/gits/mlkit`.
4. The `mlkit-bench` tool is compiled and available as `../src/mlkit-bench` (write `make all` in `..`).
5. The `mlkit-bench-press` tool is compiled and available as `../src/press/mlkit-bench-press` (write `smlpkg sync` in `../src/charting` and `make all` in `../src/press/`).

Geee - sorry - I need to automate the above stuff further...
