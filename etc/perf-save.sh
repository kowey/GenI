#!/bin/sh

cd `dirname $0`/..
darcs record
darcs push ../PerfStableGeni

echo "------ running perf STABLE geni -------"
cd ../PerfStableGeni
make debugger
./debugger-geni +RTS -p -K50M > out-stable
mv debugger-geni.prof prof-stable
