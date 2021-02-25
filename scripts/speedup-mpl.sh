#!/bin/sh

set -e

source $(dirname $0)/speedup.config

baseline=baseline-mpl.json
speedups=mpl.data

cores_json() {
    echo mpl-${1}.json
}

measure_baseline() {
    $bench -o $baseline -mlton:-mlb-path-var \'MLCOMP mlton\': $(benchmarks)
}

measure_parallel() {
    for c in $(seq $cores); do
        restrict() {
            taskset -c 0-$((c-1)) "$@"
        }
        restrict $bench -o $(cores_json $c) -mpl:-mlb-path-var \'MLCOMP mpl\' -runtime procs -runtime $c: $(benchmarks)
    done
}

compute_speedups() {
    json_args() {
        for c in $(seq $cores); do
            echo "-$c" $(cores_json $c)
        done
    }
    $speedup -baseline $baseline $(json_args) $(benchmarks) > $speedups
}

plot_speedups() {
    gnuplot -e "$(gnuplot_prelude "mpl" $speedups)" -d $BENCH_ROOT/scripts/speedup.gnu
}

measure_baseline
measure_parallel
compute_speedups
plot_speedups
