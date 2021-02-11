## Parallel Benchmarks

Notice: See below for prerequisites to running `make press`...

Here is the output from running `make press` (headers slightly
modified) on a MacBook Pro (15-inch, 2016) with a 2,7 GHz Quad-Core
Intel Core i7 CPU:

```
 |          | MLton 20201023 | MLKitSeq      | MLKitPar - 1 Core | MLKitPar - 4 Cores |
 | Program  | real time (s)  | real time (s) | real time (s)     | real time (s)      |
 +----------+----------------+---------------+-------------------+--------------------+
 | fib      | 0.51 ±0.7%     | 0.49 ±1.5%    | 0.53 ±2.1%        | 0.11 ±3.1%         |
 | nqueens  | 1.30 ±1.6%     | 1.87 ±1.7%    | 1.94 ±1.7%        | 0.86 ±0.8%         |
 | pmsort   | 0.48 ±1.6%     | 0.19 ±0.0%    | 0.45 ±1.1%        | 0.15 ±0.0%         |
 | primes   | 0.92 ±1.9%     | 2.24 ±1.1%    | 2.38 ±1.7%        | 0.56 ±3.1%         |
 | ray-orig | 4.06 ±1.5%     | 8.20 ±1.5%    | 17.24 ±0.7%       | 4.34 ±1.4%         |
 | ray      | 4.00 ±1.1%     | 2.37 ±0.7%    | 2.46 ±2.6%        | 0.57 ±1.8%         |
 | filter   | 0.31 ±1.1%     | 0.57 ±2.1%    | 0.56 ±1.7%        | 0.22 ±4.1%         |
 | scan     | 0.47 ±2.1%     | 0.74 ±2.7%    | 0.74 ±1.5%        | 0.24 ±3.9%         |
 | sgm_scan | 0.12 ±4.3%     | 0.24 ±0.0%    | 0.25 ±2.1%        | 0.54 ±4.8%         |
```

We need to investigate the overhead of the parallel runtime and the
parallel generated code for the `pmsort` benchmark and the `ray-orig`
benchmark.

Please notice that some of the benchmarks (especially the `ray` benchmark)
have been hand-optimised for running well with MLKit (`ray-orig` is
Troels Henriksen's original version). In particular, for the `ray` benchmark,
special blocks of unboxed reals are used for avoiding the construction
of tuples of boxed reals. We see that MLton's whole-program
optimisation techniques do not result in different execution times for
the modified and the original version of the `ray` benchmark.

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
