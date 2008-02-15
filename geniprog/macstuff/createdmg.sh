#!/bin/sh
size=30m
raw=geniraw.dmg
final=geni.dmg

name='GenI generator'

rm ${raw} ${final} 

hdiutil create -size ${size} -fs HFS+ -volname "${name}" ${raw} 
DEVS=$(hdiutil attach ${raw} | cut -f 1)
DEV=$(echo $DEVS | cut -f 1 -d ' ')
cp -R bin/geni.app examples "/Volumes/${name}"
hdiutil detach $DEV
hdiutil convert ${raw} -format UDZO -o ${final} 
