#!/bin/sh

GRAMMAR=headSem

cd gram
MetaTAG ${GRAMMAR}.mg --chk -c ${GRAMMAR}.rec 
