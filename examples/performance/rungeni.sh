#!/bin/bash
TC=$1
PROF="+RTS -hc -K100M -i0.01 -sstderr -RTS"
../../dist/build/geni/geni -m macros -l lexicon -s suite --rootfeat='[cat:x]' --detect-pols='cat' --opt='p f-sem f-root' --nogui --testcase=$TC $PROF
mv geni.hp geni-$TC.hp
hp2ps -c geni-$TC.hp
#gv -orientation=seascape geni-$TC.ps
#+RTS -K120388608
