#!/bin/sh

BUNDLE=bin/geni.app
FRAMEWORKS_DIR=${BUNDLE}/Contents/Frameworks
mkdir -p ${FRAMEWORKS_DIR} 

RESBIN=${BUNDLE}/Contents/Resources/bin
mkdir -p ${RESBIN}
cp /opt/local/bin/dot ${RESBIN}


cp_the_lib () {
  LIB=$1
  BINARY=$2
  NAME=`basename $1`
  NEW_PATH="$3/${NAME}"

  echo "Copying ${NAME} to private framework"
  cp ${LIB} ${FRAMEWORKS_DIR}
  install_name_tool -id ${NEW_PATH} ${FRAMEWORKS_DIR}/${NAME}
  install_name_tool -change ${LIB} ${NEW_PATH} ${BINARY}
}

set_the_lib () {
  LIB=$1
  BINARY=$2
  NAME=`basename $1`
  NEW_PATH="$3/${NAME}"

  echo "Renaming ${NAME} in library ${BINARY}"
  install_name_tool -change ${LIB} ${NEW_PATH} ${BINARY}
}


cp_the_lib_geni () {
  cp_the_lib ${1} ${BUNDLE}/Contents/MacOS/geni\
    @executable_path/../Frameworks
}

cp_the_lib_dot () {
  cp_the_lib ${1} ${BUNDLE}/Contents/Resources/bin/dot\
    @executable_path/../../Frameworks
}

GENI_LIBS="/opt/local/lib/libgmp.3.dylib\
  /Volumes/Data/Users/kow/tmp/STATIC/lib/libwxc-mac2.6.2-0.9.4.dylib
"

DOT_LIBS="/opt/local/lib/graphviz/libcommon.0.dylib\
 /opt/local/lib/graphviz/libgvrender.0.dylib \
 /opt/local/lib/graphviz/libgvgd.2.dylib\
 /opt/local/lib/libiconv.2.dylib\
 /opt/local/lib/libjpeg.62.dylib\
 /opt/local/lib/libpng.3.dylib\
 /opt/local/lib/libz.1.dylib \
 /opt/local/lib/graphviz/libdotgen.0.dylib\
 /opt/local/lib/graphviz/libpathplan.0.dylib\
 /opt/local/lib/graphviz/libgraph.0.dylib \
 /opt/local/lib/graphviz/libcdt.0.dylib"
 
for i in ${GENI_LIBS}; do
  cp_the_lib_geni ${i}
done


for i in ${DOT_LIBS}; do
  cp_the_lib_dot ${i}
done

for i in ${DOT_LIBS}; do
  for j in ${FRAMEWORKS_DIR}/*.dylib; do
    set_the_lib ${i} ${j} @executable_path/../../Frameworks
  done
done


