#!/bin/sh

GDEPATH=${HOME}/phd/tools/GDE

LEXDIR=lex
TMPLEX=${LEXDIR}/tmp
LEX_COMM=${LEXDIR}/commonLexicon.lex

touch $TMPLEX
for i in ${LEXDIR}/*.lex; do
  LEX=`basename ${i} .lex`
  LEX_IN=${LEXDIR}/${LEX}.lex
  LEX_OUT=prefil/${LEX}.prefil
  if [ ! -e $LEX_OUT -o\
       -n "`find ${LEX_IN} -newer ${LEX_OUT} -print`" -o\
       -n "`find ${LEX_COMM} -newer ${LEX_OUT} -print`" ]
  then
    echo "rebuilding ${LEX_OUT} because ${LEX_IN} or ${LEX_COMM} was modified"
    rm ${TMPLEX}
    cat ${LEX_IN} ${LEX_COMM} > ${TMPLEX}
    ${GDEPATH}/runLexicalTranslator.sh -a ${TMPLEX} ${LEX_OUT}
  else
   echo "skipping ${LEX_OUT} (up to date already)"
  fi 
done

