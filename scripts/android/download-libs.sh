#!/bin/bash

set -e

function readlink() {
  echo "$(cd "$(dirname "$1")"; pwd -P)"
}

if [ -z "${1}" ]; then
    echo "Job repo is unset. Provide it via first argument like: $(readlink "$0")/download-libs.sh https://something.com/job/something/{master,stable}"
    exit 1
fi

job_repo=$1
default_arch=$2

arches=("aarch64" "armv7a")
output_arches=("arm64-v8a" "armeabi-v7a")

if [ -z "${default_arch}" ]; then
    # No custom architectures were specified, using defaults
    echo "Libs for all supported architectures will be downloaded. To use single arch, pass one of the following values to the end of command: ${arches[*]}"
else
    for ((i = 0 ; i < ${#output_arches[@]}; i++)); do
        if [ "${arches[$i]}" == "$default_arch" ]; then
          output_arches=("${output_arches[$i]}")
        fi
    done
    arches=("$default_arch")
fi

root_dir="$(dirname "$(dirname "$(readlink "$0")")")"
for ((i = 0 ; i < ${#arches[@]}; i++)); do
    arch="${arches[$i]}"
    output_arch="${output_arches[$i]}"
    output_dir="$root_dir/apps/multiplatform/common/src/commonMain/cpp/android/libs/$output_arch/"

    mkdir -p "$output_dir" 2> /dev/null

    curl --tlsv1.2 --location -o libsupport.zip $job_repo/x86_64-linux."$arch"-android:lib:support/latest/download/1 && \
    unzip -o libsupport.zip && \
    mv libsupport.so "$output_dir" && \
    rm libsupport.zip

    curl --tlsv1.2 --location -o libsimplex.zip "$job_repo"/x86_64-linux."$arch"-android:lib:simplex-chat/latest/download/1 && \
    unzip -o libsimplex.zip && \
    mv libsimplex.so "$output_dir" && \
    rm libsimplex.zip
done
