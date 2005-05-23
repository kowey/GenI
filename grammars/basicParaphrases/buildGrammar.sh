#!/bin/sh

GRAMMAR=headSem
GRAMMAR_TMP=${GRAMMAR}.tmp

cd gram
MetaTAG ${GRAMMAR}.mg       --chk -c ${GRAMMAR_TMP}.rec 
CheckTAG ${GRAMMAR_TMP}.rec --chk -c ${GRAMMAR}.rec 
rm ${GRAMMAR_TMP}.rec
