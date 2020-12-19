## Parallel Benchmarks

Here is the output from running `make press` on a MacBook Pro
(15-inch, 2016) with a 2,7 GHz Quad-Core Intel Core i7 CPU:

```
$ make press
...
| pname               | user       | real       | rss         |
| fib/fib.mlb         | 0.81 ±0.6% | 0.11 ±4.7% | 2.2M ±1.0%  |
| nqueens/nqueens.mlb | 4.94 ±0.8% | 0.96 ±1.7% | 167M ±0.4%  |
| pmsort/pmsort.mlb   | 0.71 ±1.5% | 0.25 ±4.2% | 68.8M ±1.5% |
| ray/ray.mlb         | 4.11 ±0.3% | 0.54 ±0.9% | 41.9M ±0.6% |
```
