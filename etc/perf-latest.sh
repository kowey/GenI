#!/bin/sh

OUT_FILE=profout
PROF_FILE=prof
STABLE_DIR=../PerfGeniStable
STABLE_OUT_FILE=profout-stable
STABLE_PROF_FILE=profout-stable

cd `dirname $0`/..

make debugger
# quit if make fails
MAKE_STATUS=$?
if [ "${MAKE_STATUS}" -ne 0 ] ; then exit 1; fi

echo "Running GenI ..."
make profiler
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

