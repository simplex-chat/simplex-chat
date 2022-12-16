#!/bin/bash

set -e

function readlink() {
  echo $(cd $(dirname $1); pwd -P)
}

if [ -z ${1} ]; then
    echo "Job repo is unset. Provide it via first argument like: $(readlink $0)/download_libs_aarch64.sh https://something.com/job/something"
    exit 1
fi

job_repo=$1
arch="aarch64"
#arch="x86_64"
output_arch="arm64-v8a"
#output_arch="x86_64"

root_dir="$(dirname $(dirname $(readlink $0)))"
output_dir="$root_dir/apps/android/app/src/main/cpp/libs/$output_arch/"

mkdir -p "$output_dir" 2> /dev/null

curl --location -o libsupport.zip $job_repo/simplex-chat/$arch-android:lib:support.x86_64-linux/latest/download/1 && \
unzip -o libsupport.zip && \
mv libsupport.so "$output_dir" && \
rm libsupport.zip

curl --location -o libsimplex.zip $job_repo/simplex-chat/$arch-android:lib:simplex-chat.x86_64-linux/latest/download/1 && \
unzip -o libsimplex.zip && \
mv libsimplex.so "$output_dir" && \
rm libsimplex.zip
