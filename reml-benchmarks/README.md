## Parallel Benchmarks

Notice: See below for prerequisites to running `make press`...

Here is the output from running `make press` (headers slightly
modified) on a MacBook Pro (15-inch, 2016) with a 2,7 GHz Quad-Core
Intel Core i7 CPU:

```
 |             | MLton 20201023 | MLKitSeq      | MLKitPar - 1 Core | MLKitPar - 4 Cores | Prog |
 | Program     | real time (s)  | real time (s) | real time (s)     | real time (s)      | len  |
 +-------------+----------------+---------------+-------------------+--------------------+------+
 | fib         | 0.55 ±2.6%     | 0.51 ±2.0%    | 0.51 ±1.7%        | 0.12 ±4.3%         | 132  |
 | mandelbrot  | 1.34 ±0.4%     | 1.38 ±6.4%    | 1.29 ±1.6%        | 0.36 ±5.2%         | 319  |
 | nqueens     | 1.38 ±0.4%     | 1.76 ±2.8%    | 2.03 ±2.1%        | 0.89 ±1.1%         | 212  |
 | pmsort      | 0.51 ±3.1%     | 0.19 ±0.0%    | 0.45 ±1.7%        | 0.16 ±2.8%         | 178  |
 | primes      | 0.97 ±0.5%     | 2.20 ±2.8%    | 2.47 ±2.6%        | 0.50 ±1.4%         | 584  |
 | ray-orig    | 4.20 ±0.4%     | 8.76 ±1.1%    | 18.28 ±1.1%       | 4.77 ±1.9%         | 547  |
 | ray         | 4.28 ±0.7%     | 2.56 ±2.9%    | 2.48 ±1.5%        | 0.62 ±9.1%         | 614  |
 | filter      | 0.33 ±4.3%     | 0.61 ±7.6%    | 0.56 ±0.8%        | 0.22 ±5.6%         | 563  |
 | scan        | 0.47 ±2.5%     | 0.74 ±5.6%    | 0.72 ±1.2%        | 0.22 ±4.1%         | 566  |
 | sgm_scan    | 0.13 ±4.2%     | 0.23 ±2.2%    | 0.25 ±2.7%        | 0.56 ±4.5%         | 563  |
 | soboloption | 0.31 ±2.3%     | 0.57 ±1.7%    | 0.94 ±1.3%        | 0.22 ±4.7%         | 889  |
 | sobolpi     | 0.54 ±1.6%     | 0.99 ±5.9%    | 1.29 ±1.2%        | 0.31 ±2.5%         | 854  |
 | vpmsort     | 0.27 ±5.2%     | 0.32 ±1.6%    | 0.39 ±1.1%        | 0.17 ±3.2%         | 198  |
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
than one thread. Here is a comparison of using the unsafe allocation
strategy for the `pmsort` benchmark compared to the safe version:

```
 |             | MLton 20201023 | MLKitSeq      | MLKitPar - 1 Core | MLKitPar - 4 Cores | MLKitPar0 - 1 Core | MLKitPar0 - 4 Cores |
 | Program     | real time (s)  | real time (s) | real time (s)     | real time (s)      | real time (s)      | real time (s)       |
 +-------------+----------------+---------------+-------------------+--------------------+--------------------+---------------------+
 | pmsort 1M   | 1.0288         | 0.8965        | 0.9380            | 0.2850             | 0.5223             | 0.1847              |
```

Here are the executions:

```
$ cd pmsort
$ make clean pmsort-mlton.exe pmsort-seq.exe pmsort-par.exe pmsort-par0.exe

$ ./pmsort-mlton.exe -N 1000000 --t         # MLTON
[Sorting: Finished in 1.0288s]

$ ./pmsort-seq.exe -N 1000000 --t           # SEQ
[Sorting: Finished in 0.9280s]

$ ./pmsort-par.exe -N 1000000 --t -P 0      # PAR 1-core
[Sorting: Finished in 0.9380s]

$ ./pmsort-par.exe -N 1000000 --t           # PAR 4-cores
[Sorting: Finished in 0.2850s]

$ ./pmsort-par0.exe -N 1000000 --t -P 0     # PAR0 1-core
[Sorting: Finished in 0.5223s]

$ ./pmsort-par0.exe -N 1000000 --t          # PAR0 4-core
[Sorting: Finished in 0.1847s]
```

There seems to be something wrong with the `SEQ` version...

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
