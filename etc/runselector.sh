#!/bin/sh

GRAMMAR_FILE=${1}
TMP_FIL_FILE="/tmp/geniselect_to.$$.fil"
TMP_RES_FILE="/tmp/geniselect_from.$$.xml"

# write the stuff from stdin into a filter file
cat > ${TMP_FIL_FILE}

# run the selector
SelectTAG ${GRAMMAR_FILE} ${TMP_FIL_FILE} --geni -o ${TMP_RES_FILE} > /dev/null

cat ${TMP_RES_FILE}

# convert the selector output
#`dirname $0`/../geniconvert --trees < ${TMP_RES_FILE} 

# rm ${TMP_FIL_FILE} ${TMP_RES_FILE}
