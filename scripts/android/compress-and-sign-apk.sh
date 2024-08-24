#!/bin/bash

# Fail fast in case any command fails
set -e

level=$1
apk_parent_dir=$2
sdk_dir=$3

store_file=$4
store_password=$5
key_alias=$6
key_password=$7

if [ -z "${7}" ]; then echo "You didn't enter all required params:
compress-and-sign-apk.sh level apk_parent_dir sdk_dir store_file store_password key_alias key_password"
fi

cd "$apk_parent_dir"

touch remove_this_file remove_this_FILE
(( $(ls | grep "remove_this" | wc -l)==1 )) && case_insensitive=1 || case_insensitive=0
#echo Case-insensitive file system: $case_insensitive
rm remove_this_file remove_this_FILE 2> /dev/null || true

ORIG_NAMES=( $(echo android*.apk) )
for ORIG_NAME in "${ORIG_NAMES[@]}"; do
    unzip -o -q -d apk $ORIG_NAME
    ORIG_NAME_COPY=$ORIG_NAME-copy
    mv "$ORIG_NAME" "$ORIG_NAME_COPY"

    (cd apk && zip -r -q -"$level" ../"$ORIG_NAME" .)
    # Shouldn't be compressed because of Android requirement
    (cd apk && zip -r -q -0 ../"$ORIG_NAME" resources.arsc)

    if [ $case_insensitive -eq 1 ]; then
        # For case-insensitive file systems
        list_of_files=$(unzip -l "$ORIG_NAME_COPY" | grep res/ | sed -e "s|.*res/|res/|")
        for file in $list_of_files; do unzip -o -q -d apk "$ORIG_NAME_COPY" "$file" && (cd apk && zip -r -q -0 ../"$ORIG_NAME" "$file"); done
    else
        # This method is not working correctly on case-insensitive file systems since Android AAPT produce the same names of files
        # but with different case like xX.png, Xx.png, xx.png, etc
        (cd apk && zip -r -q -0 ../"$ORIG_NAME" res)
    fi

    #(cd apk && 7z a -r -mx=$level -tzip -x!resources.arsc ../$ORIG_NAME .)
    #(cd apk && 7z a -r -mx=0 -tzip ../$ORIG_NAME resources.arsc)

    ALL_TOOLS=("$sdk_dir"/build-tools/*/)
    BIN_DIR="${ALL_TOOLS[${#ALL_TOOLS[@]}-1]}"

    "$BIN_DIR"/zipalign -p -f 4 "$ORIG_NAME" "$ORIG_NAME"-2

    mv "$ORIG_NAME"{-2,}

    "$BIN_DIR"/apksigner sign \
      --ks "$store_file" --ks-key-alias "$key_alias" --ks-pass "pass:$store_password" \
      --key-pass "pass:$key_password" "$ORIG_NAME"

    # cleanup
    rm "$ORIG_NAME_COPY" 2> /dev/null || true
    rm -rf apk || true
    rm "${ORIG_NAME}".idsig 2> /dev/null || true
done