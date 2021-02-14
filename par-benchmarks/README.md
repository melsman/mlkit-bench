## Parallel Benchmarks

Notice: See below for prerequisites to running `make press`...

Here is the output from running `make press` (headers slightly
modified) on a MacBook Pro (15-inch, 2016) with a 2,7 GHz Quad-Core
Intel Core i7 CPU:

```
 |             | MLton 20201023 | MLKitSeq      | MLKitPar - 1 Core | MLKitPar - 4 Cores |
 | Program     | real time (s)  | real time (s) | real time (s)     | real time (s)      |
 +-------------+----------------+---------------+-------------------+--------------------+
 | fib         | 0.52 ±3.4%     | 0.49 ±1.1%    | 0.53 ±1.3%        | 0.11 ±0.0%         |
 | mandelbrot  | 1.26 ±1.7%     | 1.24 ±0.9%    | 1.25 ±1.3%        | 0.36 ±7.0%         |
 | nqueens     | 1.30 ±3.2%     | 1.83 ±0.9%    | 1.85 ±0.8%        | 0.89 ±0.8%         |
 | pmsort      | 0.47 ±1.7%     | 0.19 ±0.0%    | 0.45 ±1.0%        | 0.15 ±3.4%         |
 | primes      | 0.90 ±0.8%     | 2.23 ±0.9%    | 2.30 ±1.0%        | 0.55 ±1.2%         |
 | ray-orig    | 3.87 ±0.5%     | 8.07 ±0.4%    | 17.08 ±0.8%       | 4.59 ±1.6%         |
 | ray         | 3.93 ±0.6%     | 2.41 ±2.1%    | 2.46 ±1.7%        | 0.56 ±0.6%         |
 | filter      | 0.31 ±1.6%     | 0.56 ±2.5%    | 0.57 ±1.8%        | 0.19 ±2.7%         |
 | scan        | 0.47 ±2.4%     | 0.73 ±1.5%    | 0.76 ±1.2%        | 0.22 ±2.7%         |
 | sgm_scan    | 0.11 ±4.4%     | 0.23 ±1.9%    | 0.24 ±1.4%        | 0.57 ±5.1%         |
 | soboloption | 0.29 ±1.5%     | 0.55 ±1.3%    | 0.94 ±1.6%        | 0.25 ±8.1%         |
 | sobolpi     | 0.44 ±1.1%     | 0.85 ±2.5%    | 1.39 ±2.3%        | 0.28 ±3.3%         |
```

We need to investigate the overhead of the parallel runtime and the
parallel generated code for the `pmsort` benchmark and the `ray-orig`
benchmark. These benchmarks allocate many small values in infinite
regions (pairs, etc) and it turns out that the more involved
allocation instruction sequence slows down these benchmarks. If we
could somehow identify that a region is allocated into only by a
single thread, we could choose the more efficient allocation
instruction sequences for these regions. The `pmsort` benchmark, for
instance, has the property that no region is allocated into by more
than one thread.

Please notice that some of the benchmarks (especially the `ray`
benchmark) have been hand-optimised for running well with MLKit
(`ray-orig` is Troels Henriksen's original version). In particular,
for the `ray` benchmark, special blocks of unboxed reals are used for
avoiding the construction of tuples of boxed reals. We see that
MLton's whole-program optimisation techniques do not result in
different execution times for the modified and the original version of
the `ray` benchmark.

The `scan`, `filter`, and `sgm_scan` benchmarks make use of the
`util/soac.sml` library, which implements a series of data-parallel
combinators on pull-arrays and ordinary arrays.

### Prerequisites

Running these benchmarks (using `make press`) assumes the following:

1. MLton is installed and available as a command `mlton`.
2. The `smlpkg` tool is available as a command `smlpkg`.
3. A new version of MLKit is installed. You can set the root of the installation using the `MLKIT_ROOT` environment variable (the default location is `~/gits/mlkit`.
4. The `mlkit-bench` tool is compiled and available as `../src/mlkit-bench` (write `make all` in `..`).
5. The `mlkit-bench-press` tool is compiled and available as `../src/press/mlkit-bench-press` (write `smlpkg sync` in `../src/charting` and `make all` in `../src/press/`).

Geee - sorry - I need to automate the above stuff further...

### Simple compilation

Some of the folders have `Makefile`s that allow for easy compilation. For instance, you may do as follows:

```
$ cd pmsort
$ make pmsort-par.exe
$ ./pmsort-par.exe --t
...
```
