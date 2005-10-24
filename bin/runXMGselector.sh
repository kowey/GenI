#!/bin/sh

GRAMMAR_FILE=${1}
USERNAME=`whoami`

TMP_FILE_PREFIX="/tmp/geni_to_sel-"${USERNAME}
TMP_RES_PREFIX="/tmp/geni_from_sel-"${USERNAME}

TMP_FIL_FILE="${TMP_FILE_PREFIX}-$$.fil"
TMP_RES_FILE="${TMP_RES_PREFIX}-$$.geni"

# clean up the old fil/res files
rm ${TMP_FILE_PREFIX}* ${TMP_RES_PREFIX}*

# write the stuff from stdin into a filter file
cat > ${TMP_FIL_FILE}

# run the selector
SelectTAG ${GRAMMAR_FILE} ${TMP_FIL_FILE} --geni -o ${TMP_RES_FILE}
CMDEXIT=$?

#if [ -r ${TMP_RES_FILE} ]; then
cat ${TMP_RES_FILE}
#else
#  exit 1
#fi

# convert the selector output
#`dirname $0`/../geniconvert --trees < ${TMP_RES_FILE} 

# exit with the exit status of the command we ran
#exit ${CMDEXIT}

