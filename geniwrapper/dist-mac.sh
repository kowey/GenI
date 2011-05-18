#!/bin/bash
set -e
GHC_VER=7.0.3
GHC_DIR=/Library/Frameworks/GHC.framework/Versions/7.0.3-i386/usr/lib/ghc-$GHC_VER
LOCAL_DIR=${HOME}/.cabal

DIST_DIR=MinimalGenI-OSX

make MinimalGenI.dylib

rm -rf $DIST_DIR
mkdir $DIST_DIR
cp -R ../examples/ej test-c.c *.h MinimalGenI.dylib $DIST_DIR
cp ej-sem $DIST_DIR/ej/sem
cp $(dirname $0)/{test-mac,clean-mac}.sh $DIST_DIR

echo >&2 "Copying header files"
for d in $(find $GHC_DIR -name include); do\
  cp -R $d/* $DIST_DIR
done

echo >&2 "Copying global static libraries"
for x in\
  ${GHC_DIR}/libHSrts.a ${GHC_DIR}/libHSffi.a ${GHC_DIR}/libHSrtsmain.a\
  ${GHC_DIR}/base-4.3.1.0/lib*.a \
  ${GHC_DIR}/ghc-prim-0.2.0.0/lib*.a \
  ${GHC_DIR}/integer-gmp-0.2.0.3/lib*.a \
; do\
  cp $x $DIST_DIR
done

echo >&2 "Copying global packages"
for d in \
${GHC_DIR}/array-0.3.0.2 \
${GHC_DIR}/base-4.3.1.0 \
${GHC_DIR}/bytestring-0.9.1.10 \
${GHC_DIR}/containers-0.4.0.0 \
${GHC_DIR}/directory-1.1.0.0 \
${GHC_DIR}/extensible-exceptions-0.1.1.2 \
${GHC_DIR}/filepath-1.2.0.0 \
${GHC_DIR}/ghc-${GHC_VER} \
${GHC_DIR}/ghc-prim-0.2.0.0 \
${GHC_DIR}/haskell2010-1.0.0.0 \
${GHC_DIR}/hpc-0.5.0.6 \
${GHC_DIR}/integer-gmp-0.2.0.3 \
${GHC_DIR} \
${GHC_DIR}/old-locale-1.0.0.2 \
${GHC_DIR}/old-time-1.0.0.6 \
${GHC_DIR}/pretty-1.0.1.2 \
${GHC_DIR}/process-1.0.1.5 \
${GHC_DIR}/time-1.2.0.3 \
${GHC_DIR}/unix-2.4.2.0 \
; do cp $d/*.dylib $DIST_DIR
done

echo >&2 "Copying local packages"
# Local packages used by GenI
for p in\
    HUnit-1.2.2.3\
    MaybeT-0.1.2\
    binary-0.5.0.2\
    deepseq-1.1.0.2\
    json-0.4.4\
    mtl-2.0.1.0\
    parsec-3.1.1\
    split-0.1.4\
    uniplate-1.6\
    utf8-string-0.3.6\
    syb-0.3\
    transformers-0.2.2\
    GenI-0.21\
; do cp $LOCAL_DIR/lib/$p/ghc-$GHC_VER/lib*.dylib $DIST_DIR
done

echo >&2 "Tarring up $DIST_DIR.tar.gz"
rm -f $DIST_DIR.tar.gz
tar czf $DIST_DIR.tar.gz $DIST_DIR
