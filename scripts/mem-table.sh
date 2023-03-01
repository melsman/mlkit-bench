#!/bin/sh

set -e

[ $# = 2 ]

MLKIT_RESULTS=$1
MPL_RESULTS=$2

source $(dirname $0)/speedup.config

MLKIT_BASELINE=$MLKIT_RESULTS/baseline-mlkit.json
MLKIT_RUN_1=$MLKIT_RESULTS/mlkit-1.json
MLKIT_RUN_20=$MLKIT_RESULTS/mlkit-20.json
MPL_BASELINE=$MPL_RESULTS/baseline-mpl.json
MPL_RUN_1=$MPL_RESULTS/mpl-1.json
MPL_RUN_20=$MPL_RESULTS/mpl-20.json

$mem -json $MLKIT_BASELINE,$MLKIT_RUN_1,$MLKIT_RUN_20 -json $MPL_BASELINE,$MPL_RUN_1,$MPL_RUN_20 $(benchmarks)
