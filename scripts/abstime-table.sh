#!/bin/sh

set -e

[ $# = 2 ]

MLKIT_RESULTS=$1
MPL_RESULTS=$2

source $(dirname $0)/speedup.config

MLKIT_BASELINE=$MLKIT_RESULTS/baseline-mlkit.json
MLKIT_1=$MLKIT_RESULTS/mlkit-1.json
MLKIT_20=$MLKIT_RESULTS/mlkit-20.json
MPL_BASELINE=$MPL_RESULTS/baseline-mpl.json
MPL_1=$MPL_RESULTS/mpl-1.json
MPL_20=$MPL_RESULTS/mpl-20.json

$abstime -json $MLKIT_BASELINE -json $MLKIT_1 -json $MLKIT_20 -json $MPL_BASELINE -json $MPL_1 -json $MPL_20 $(benchmarks)
