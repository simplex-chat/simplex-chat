#!/bin/bash

set -e

if [ -z ${1} ]; then
    echo "Job repo is unset. Provide it via first argument like: $(realpath $0) https://something.com/job/something"
    exit 1
fi

job_repo=$1
root_dir="$(dirname $(dirname $(dirname $(realpath $0))))"
output_dir="$root_dir/apps/android/app/src/main/cpp/libs/arm64-v8a/"

echo "$output_dir"

mkdir -p "$output_dir" 2> /dev/null

wget -O libsupport.zip $job_repo/simplex-chat-nix-android/aarch64-android:lib:support.x86_64-linux/latest/download/1
unzip -o libsupport.zip
mv libsupport.so "./$output_dir"
rm libsupport.zip

wget -O libsimplex.zip $job_repo/simplex-chat-nix-android/aarch64-android:lib:simplex-chat.x86_64-linux/latest/download/1
unzip -o libsimplex.zip
mv libsimplex.so "./$output_dir"
rm libsimplex.zip
