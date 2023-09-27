#!/bin/bash
set -e

lib=libapp-lib.dylib
RPATHS=$(otool -l $lib | grep -E '/Users|/opt/|/usr/local' | cut -d' ' -f11)
for RPATH in $RPATHS; do
    install_name_tool -delete_rpath $RPATH $lib
done
