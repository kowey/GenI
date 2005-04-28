#!/bin/sh

GRAMMAR_FILE=${1}
TMP_FIL_FILE="/tmp/geniselect_to.$$.fil"
TMP_RES_FILE="/tmp/geniselect_from.$$.xml"
TMP_CONV_FILE="/tmp/geniselect_conv.$$.geni"

# write the stuff from stdin into a filter file
cat > ${TMP_FIL_FILE}

# run the selector
SelectTAG ${GRAMMAR_FILE} ${TMP_FIL_FILE} --xml -o ${TMP_RES_FILE} > /dev/null

# convert the selector output
TMP_RES_FILE="/tmp/geniselect_from.15261.xml"
`dirname $0`/../geniconvert --trees < ${TMP_RES_FILE} > ${TMP_CONV_FILE}
cat ${TMP_CONV_FILE}

# rm ${TMP_FIL_FILE} ${TMP_RES_FILE}
