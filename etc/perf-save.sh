#!/bin/sh

cd `dirname $0`/..
darcs record
darcs push ../PerfStableGeni

cd ../PerfStableGeni
echo "------ making perf STABLE geni -------"
make debugger
echo "------ running perf STABLE geni -------"
./debugger-geni +RTS -p -K50M > out-stable
mv debugger-geni.prof prof-stable
