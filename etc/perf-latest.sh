#!/bin/sh

OUT_FILE=profout
PROF_FILE=prof
STABLE_DIR=../PerfGeniStable
STABLE_OUT_FILE=profout-stable
STABLE_PROF_FILE=prof-stable
PROFILE_WITH=-hc
HEAP_FILE=debugger-geni${PROFILE_WITH}.txt

cd `dirname $0`/..

make debugger
# quit if make fails
MAKE_STATUS=$?
if [ "${MAKE_STATUS}" -ne 0 ] ; then exit 1; fi

echo "Running GenI ..."
make profiler PROFILE_WITH=${PROFILE_WITH}
mv debugger-geni.prof ${PROF_FILE}

echo "----- first make sure you have the same results -----"
head ${STABLE_DIR}/${STABLE_OUT_FILE} > outhead-stable
head ${OUT_FILE} > outhead
diff outhead-stable outhead | grep -v Loading
echo "----- here are the cost centre results -----"
echo "== BEFORE =="
head ${STABLE_DIR}/${STABLE_PROF_FILE}
echo "== AFTER =="
head ${PROF_FILE}
echo "----- here are the heap results -----"
echo "== BEFORE =="
cat ${STABLE_DIR}/${HEAP_FILE}
echo "== AFTER =="
cat ${HEAP_FILE}
echo "== DIFF =="
diff ${STABLE_DIR}/${HEAP_FILE} ${HEAP_FILE}
