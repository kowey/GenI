#!/bin/sh

SUFFIX=${1}
STEM=profres-${SUFFIX}

cd `dirname $0`/..

rm debugger-geni-${SUFFIX}.hp
./debugger-geni +RTS -K50m -${SUFFIX}
mv debugger-geni.hp ${STEM}.hp
sed -e "/alex/d" -e "/parser/d" ${STEM}.hp > ${STEM}-trim.hp
hp2ps ${STEM}-trim.hp
