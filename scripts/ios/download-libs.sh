#!/bin/bash

set -e

function readlink() {
  echo "$(cd "$(dirname "$1")"; pwd -P)"
}

if [ -z "${1}" ]; then
    echo "Job repo is unset. Provide it via first argument like: $(readlink "$0")/download_libs.sh https://something.com/job/something/{master,stable}"
    exit 1
fi

job_repo=$1
default_arch=$2

arches=("aarch64" "x86_64")
output_arches=("aarch64" "x86_64")

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
    output_dir="$HOME/Downloads"

    curl --tlsv1.2 --location -o "$output_dir"/pkg-ios-"$arch"-swift-json.zip "$job_repo"/"$arch"-darwin."$arch"-darwin-ios:lib:simplex-chat/latest/download/1 && \
    unzip -o "$output_dir"/pkg-ios-"$output_arch"-swift-json.zip -d ~/Downloads/pkg-ios-"$output_arch"-swift-json
done
sh "$root_dir"/scripts/ios/prepare-x86_64.sh