#!/bin/sh

set -e

source ./speedup.config

baseline=baseline-mlkit.json
speedups=mlkit-par0.data

cores_json() {
    echo mlkit-par0-${1}.json
}

measure_parallel() {
    for c in $(seq $cores); do
        restrict() {
            taskset -c 0-$((c-1)) "$@"
        }
        restrict $bench -o $(cores_json $c) -mlkit:MLCOMP=mlkit-par -no_gc -par -mlb-subdir C1: $benchmarks
    done
}

compute_speedups() {
    json_args() {
        for c in $(seq $cores); do
            echo "-$c" $(cores_json $c)
        done
    }
    $speedup -baseline $baseline $(json_args) $benchmarks > $speedups
}

plot_speedups() {
    gnuplot -e "$(gnuplot_prelude "mlkit-par0" $speedups)" -d speedup.gnu
}

measure_parallel
compute_speedups
plot_speedups
