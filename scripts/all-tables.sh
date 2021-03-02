#!/bin/sh

set -e

basedir=/home/projects/ku_00054/data/gits
export BENCH_ROOT=$basedir/mlkit-bench

scriptdir=$BENCH_ROOT/scripts
source $scriptdir/speedup.config

$scriptdir/abstime-table.sh mlkit mpl > abstime.tex
$scriptdir/mem-table.sh mlkit mpl > mem.tex
