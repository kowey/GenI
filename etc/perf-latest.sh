#!/bin/sh

cd `dirname $0`/..

make debugger
./debugger-geni +RTS -p -K50M > out
mv debugger-geni.prof prof

echo "----- first make sure you have the same results -----"
head ../PrefGeniStable/out > outhead-stable
head out outhead
diff outhead-stable outhead
echo "----- here are the performance results -----"
head prof > profhead
head ../PerfStableGeni/prof-stable > profhead-stable.prof
diff profhead-stable profhead 
