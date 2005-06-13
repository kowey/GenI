#!/bin/sh

OUT_FILE=out
PROF_FILE=prof
STABLE_DIR=../PerfStableGeni
STABLE_OUT_FILE=out-stable
STABLE_PROF_FILE=prof-stable

cd `dirname $0`/..

make debugger
echo "Running GenI ..."
./debugger-geni +RTS -p -K50M > ${OUT_FILE} 
mv debugger-geni.prof ${PROF_FILE}

echo "----- first make sure you have the same results -----"
head ${STABLE_DIR}/${STABLE_OUT_FILE} > outhead-stable
head ${OUT_FILE} > outhead
diff outhead-stable outhead | grep -v Loading
echo "----- here are the performance results -----"
echo "== BEFORE =="
head ${STABLE_DIR}/${STABLE_PROF_FILE}
echo "== AFTER =="
head ${PROF_FILE} 

