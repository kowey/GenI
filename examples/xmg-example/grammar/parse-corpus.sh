#!/bin/sh
# shell script containing the command line options
# to use the SemConst in interactive mode
# february 2007
# contact: parmenti@loria.fr

PWD=`pwd`
RESOURCE_DIR=`dirname $PWD`

cd $RESOURCE_DIR
./SemConst.exe --interactive\
  -g ${RESOURCE_DIR}/demo/Evaluations.mg\
  -l ${RESOURCE_DIR}/demo/demo-lemma-latin1.lex\
  -m ${RESOURCE_DIR}/demo/demo-morph-latin1.mph\
  -c ${RESOURCE_DIR}/demo/demo-corpus-latin1.txt\
  -w
