#!/bin/sh
#PBS -W group_list=ku_00054 -A ku_00054
#PBS -N mlkit-bench
#PBS -l walltime=00:60:00
#PBS -l nodes=1:ppn=40,gpus=0
#PBS -l mem=64g

# Batch job for running the benchmarks on Computerome.  Submit with
#
# $ qsub -d $PWD -v compiler=<mlkit|mlkit-par0|mpl> path/to/computerome.sh
#
# (If run without -d it'll make a mess in your home directory.)
#
# NOTE: because I am lazy, mlkit-par0 will expect the mlkit baseline
# measurement to already exist.  Run the plain 'mlkit' compiler first.

set -e

basedir=/home/projects/ku_00054/data/gits
cores=40

export BENCH_ROOT=$basedir/mlkit-bench
export MLKIT_ROOT=$basedir/mlkit
export MPL_ROOT=$basedir/mpl
export MLTON_ROOT=$basedir/mpl

export PATH=$MLKIT_ROOT/bin:$MPL_ROOT/build/bin:$PATH

# We need a newer GCC for MLKit.
module load gcc
module load tools
module load gnuplot/5.4.0

echo "Compiler: $compiler"
echo "Cores: $cores"

case $compiler in
    mlkit)
        $BENCH_ROOT/scripts/speedup-mlkit.sh $cores
        ;;
    mlkit-par0)
        $BENCH_ROOT/scripts/speedup-mlkit-par0.sh $cores
        ;;
    mpl)
        $BENCH_ROOT/scripts/speedup-mpl.sh $cores
        ;;
    *)
        echo "'$1' should have been one of mlkit, mlkit-par0, mpl"
        exit 1
        ;;
esac
