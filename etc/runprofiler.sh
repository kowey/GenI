#!/bin/sh

if [ $# -lt 1 ]; then
  echo "usage: $0 [switches]"
  exit 1
fi

OPTIONS=$@
PROG=debugger-geni
SUFFIX=`date +%d-%H%M-%S`
STEM=heap-${SUFFIX}

cd `dirname $0`/..

./${PROG} +RTS -K50m ${OPTIONS}
mv ${PROG}.hp ${STEM}.hp
sed -e "/alex/d" -e "/parser/d" ${STEM}.hp > ${STEM}-trim.hp
hp2ps ${STEM}-trim.hp
if [ $? -eq 0 ]; then
  rm heap-latest.ps
  ln -s ${STEM}-trim.ps heap-latest.ps
  gv -seascape heap-latest.ps &
fi
