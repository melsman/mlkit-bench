## Parallel Benchmarks

Notice: See below for prerequisites to running `make press`...

Here is the output from running `make press` (headers slightly
modified) on a MacBook Pro (15-inch, 2016) with a 2,7 GHz Quad-Core
Intel Core i7 CPU:

```
 |            | MLton 20201023 | MLKitSeq      | MLKitPar - 1 Core | MLKitPar - 4 Cores |
 | Program    | real time (s)  | real time (s) | real time (s)     | real time (s)      |
 +------------+----------------+---------------+-------------------+--------------------+
 | fib        | 0.51 ±1.0%     | 0.51 ±3.0%    | 0.53 ±2.3%        | 0.11 ±0.0%         |
 | mandelbrot | 1.24 ±1.1%     | 1.32 ±2.4%    | 1.29 ±2.3%        | 0.35 ±2.0%         |
 | nqueens    | 1.35 ±3.0%     | 1.90 ±3.0%    | 1.88 ±2.2%        | 0.85 ±1.3%         |
 | pmsort     | 0.47 ±2.7%     | 0.21 ±11.2%   | 0.44 ±1.1%        | 0.14 ±3.1%         |
 | primes     | 0.90 ±1.2%     | 2.34 ±3.5%    | 2.32 ±2.2%        | 0.54 ±1.5%         |
 | ray-orig   | 4.40 ±5.2%     | 8.36 ±1.2%    | 17.48 ±1.3%       | 4.23 ±1.0%         |
 | ray        | 4.30 ±4.9%     | 2.54 ±3.4%    | 2.43 ±1.0%        | 0.55 ±0.0%         |
 | filter     | 0.31 ±2.2%     | 0.62 ±3.3%    | 0.55 ±1.8%        | 0.20 ±0.0%         |
 | scan       | 0.48 ±3.2%     | 0.82 ±10.4%   | 0.72 ±2.1%        | 0.22 ±3.3%         |
 | sgm_scan   | 0.11 ±3.0%     | 0.24 ±6.4%    | 0.24 ±2.1%        | 0.56 ±4.3%         |
 | mcpi       | 0.36 ±1.4%     | 0.90 ±2.1%    | 1.38 ±2.2%        | 0.27 ±1.8%         |
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
