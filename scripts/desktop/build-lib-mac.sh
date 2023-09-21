#!/bin/bash

OS=mac
ARCH="${1:-`uname -a | rev | cut -d' ' -f1 | rev`}"
GHC_VERSION=9.6.2

if [ "$ARCH" == "arm64" ]; then
    ARCH=aarch64
fi

LIB_EXT=dylib
LIB=libHSsimplex-chat-*-inplace-ghc*.$LIB_EXT
GHC_LIBS_DIR=$(ghc --print-libdir)

BUILD_DIR=dist-newstyle/build/$ARCH-*/ghc-*/simplex-chat-*

rm -rf $BUILD_DIR
cabal build lib:simplex-chat lib:simplex-chat --ghc-options="-optl-Wl,-rpath,@loader_path -optl-Wl,-L$GHC_LIBS_DIR/$ARCH-osx-ghc-$GHC_VERSION -optl-lHSrts_thr-ghc$GHC_VERSION -optl-lffi"

cd $BUILD_DIR/build
mkdir deps 2> /dev/null

# It's not included by default for some reason. Compiled lib tries to find system one but it's not always available
#cp $GHC_LIBS_DIR/libffi.dylib ./deps
(
  BUILD=$PWD
  cp /tmp/libffi-3.4.4/*-apple-darwin*/.libs/libffi.dylib $BUILD/deps || \
    ( \
    cd /tmp && \
    curl "https://gitlab.haskell.org/ghc/libffi-tarballs/-/raw/libffi-3.4.4/libffi-3.4.4.tar.gz?inline=false" -o libffi.tar.gz && \
    tar -xzvf libffi.tar.gz && \
    cd "libffi-3.4.4" && \
    ./configure && \
    make && \
    cp *-apple-darwin*/.libs/libffi.dylib $BUILD/deps \
    )
)

DYLIBS=`otool -L $LIB | grep @rpath | tail -n +2 | cut -d' ' -f 1 | cut -d'/' -f2`
RPATHS=`otool -l $LIB | grep "path "| cut -d' ' -f11`

PROCESSED_LIBS=()

function copy_deps() {
    local LIB=$1
    if [[ "${PROCESSED_LIBS[*]}" =~ "$LIB" ]]; then
    	return 0
    fi

    PROCESSED_LIBS+=$LIB
	local DYLIBS=`otool -L $LIB | grep @rpath | tail -n +2 | cut -d' ' -f 1 | cut -d'/' -f2`
	local NON_FINAL_RPATHS=`otool -l $LIB | grep "path "| cut -d' ' -f11`
	local RPATHS=`otool -l $LIB | grep "path "| cut -d' ' -f11 | sed "s|@loader_path/..|$GHC_LIBS_DIR|"`

	cp $LIB ./deps
    if [[ "$NON_FINAL_RPATHS" == *"@loader_path/.."* ]]; then
		# Need to point the lib to @loader_path instead
		install_name_tool -add_rpath @loader_path ./deps/`basename $LIB`
	fi
	#echo LIB $LIB
	#echo DYLIBS ${DYLIBS[@]}
	#echo RPATHS ${RPATHS[@]}

	for DYLIB in $DYLIBS; do
	    for RPATH in $RPATHS; do
	        if [ -f "$RPATH/$DYLIB" ]; then
	            #echo DEP IS "$RPATH/$DYLIB"
	            if [ ! -f "deps/$DYLIB" ]; then
	            	cp "$RPATH/$DYLIB" ./deps
	            fi
	            copy_deps "$RPATH/$DYLIB"
	        fi
	    done
	done
}

copy_deps $LIB
# Special case
cp $(ghc --print-libdir)/$ARCH-osx-ghc-$GHC_VERSION/libHSghc-boot-th-$GHC_VERSION-ghc$GHC_VERSION.dylib deps
rm deps/`basename $LIB`

if [ -e deps/libHSdrct-*.$LIB_EXT ]; then
    LIBCRYPTO_PATH=$(otool -l deps/libHSdrct-*.$LIB_EXT | grep libcrypto | cut -d' ' -f11)
    install_name_tool -change $LIBCRYPTO_PATH @rpath/libcrypto.1.1.$LIB_EXT deps/libHSdrct-*.$LIB_EXT
    cp $LIBCRYPTO_PATH deps/libcrypto.1.1.$LIB_EXT
    chmod 755 deps/libcrypto.1.1.$LIB_EXT
fi

cd -

rm -rf apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
rm -rf apps/multiplatform/desktop/src/jvmMain/resources/libs/$OS-$ARCH/
rm -rf apps/multiplatform/desktop/build/cmake

mkdir -p apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
cp -r $BUILD_DIR/build/deps apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
cp $BUILD_DIR/build/libHSsimplex-chat-*-inplace-ghc*.$LIB_EXT apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
scripts/desktop/prepare-vlc-mac.sh
