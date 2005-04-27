#!/bin/sh

GRAMMAR_FILE=${1}
FILTER_FILE=${2}
TMP_FILE="/tmp/fromSelectTAG.$$.xml"

SelectTAG ${GRAMMAR_FILE} ${FILTER_FILE} --xml -o ${TMP_FILE} > /dev/null
`dirname $0`/../geniconvert --trees < ${TMP_FILE} 
