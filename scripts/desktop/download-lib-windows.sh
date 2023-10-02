#!/bin/bash

set -e

function readlink() {
  echo "$(cd "$(dirname "$1")"; pwd -P)"
}

if [ -z "${1}" ]; then
    echo "Job repo is unset. Provide it via first argument like: $(readlink "$0")/download-lib-windows.sh https://something.com/job/something/{windows,windows-8107}"
    exit 1
fi

job_repo=$1
arch=x86_64
root_dir="$(dirname "$(dirname "$(readlink "$0")")")"
output_dir="$root_dir/apps/multiplatform/common/src/commonMain/cpp/desktop/libs/windows-$arch/"

mkdir -p "$output_dir"/deps 2> /dev/null

curl --location -o libsimplex.zip $job_repo/$arch-linux.$arch-windows:lib:simplex-chat/latest/download/1 && \
$WINDIR\\System32\\tar.exe -xf libsimplex.zip && \
mv libsimplex.dll "$output_dir" && \
mv libcrypto*.dll "$output_dir/deps" && \
mv libffi*.dll "$output_dir/deps" && \
mv libgmp*.dll "$output_dir/deps" && \
rm libsimplex.zip
