#!/bin/bash

# Fail fast in case any command fails
set -e

level=$1
apk_parent_dir=$2
sdk_dir=$3
build_type=$4

store_file=$5
store_password=$6
key_alias=$7
key_password=$8

if [ -z ${8} ]; then echo "You didn't enter all required params:
compress-and-sign-apk.sh level apk_parent_dir sdk_dir build_type store_file store_password key_alias key_password"
fi

cd $apk_parent_dir

ORIG_NAME=$(echo app*.apk)
unzip -o -q -d apk $ORIG_NAME

rm $ORIG_NAME

(cd apk && zip -r -q -$level ../$ORIG_NAME .)
# Shouldn't be compressed because of Android requirement
(cd apk && zip -r -q -0 ../$ORIG_NAME resources.arsc)
#(cd apk && 7z a -r -mx=$level -tzip -x!resources.arsc ../$ORIG_NAME .)
#(cd apk && 7z a -r -mx=0 -tzip ../$ORIG_NAME resources.arsc)

ALL_TOOLS=($sdk_dir/build-tools/*/)
BIN_DIR="${ALL_TOOLS[1]}"

$BIN_DIR/zipalign -p -f 4 app-${build_type}.apk app-${build_type}2.apk

mv app-${build_type}{2,}.apk

$BIN_DIR/apksigner sign \
  --ks "$store_file" --ks-key-alias "$key_alias" --ks-pass "pass:$store_password" \
  --key-pass "pass:$key_password" app-${build_type}.apk

# cleanup
rm -rf apk || true
rm ${ORIG_NAME}.idsig 2> /dev/null || true
