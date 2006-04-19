#!/bin/sh

OUT_FILE=out
PROF_FILE=prof
STABLE_DIR=../PerfGeniStable
STABLE_OUT_FILE=out-stable
STABLE_PROF_FILE=prof-stable

cd `dirname $0`/..

make debugger
# quit if make fails
MAKE_STATUS=$?
if [ "${MAKE_STATUS}" -ne 0 ] ; then exit 1; fi

echo "Running GenI ..."
bin/debugger-geni +RTS -p -RTS -s etc/perftest/semantics-t33 --nogui -m etc/perftest/lexselection-t33 --preselected --opts=pol > ${OUT_FILE}
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

