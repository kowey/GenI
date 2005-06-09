#!/bin/sh

cd `dirname $0`/..
darcs record
darcs push ../PerfGeniStable

echo "------ running perf STABLE geni -------"
cd ../PerfGeniStable
make debugger
./debugger-geni +RTS -p -K50M > out-stable
mv debugger-geni.prof prof-stable
