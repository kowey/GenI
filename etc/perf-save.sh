#!/bin/sh

cd `dirname $0`/../../PerfGeniStable
darcs pull -a
echo "------ making perf STABLE geni -------"
make debugger
echo "------ running perf STABLE geni -------"
bin/debugger-geni +RTS -p -RTS -s etc/perftest/semantics-t33 --nogui -m etc/perftest/lexselection-t33 --preselected --opts=pol > out-stable
mv debugger-geni.prof prof-stable
