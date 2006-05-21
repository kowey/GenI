#!/bin/sh

cd `dirname $0`/../../PerfGeniStable
darcs pull -a
echo "------ making perf STABLE geni -------"
make debugger
echo "------ running perf STABLE geni -------"
make profiler PROFILE_WITH=-hm
mv profout profout-stable
mv debugger-geni.prof prof-stable
