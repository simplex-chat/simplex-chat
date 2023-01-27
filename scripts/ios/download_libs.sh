#!/bin/bash

set -e

function readlink() {
  echo $(cd $(dirname $1); pwd -P)
}

if [ -z ${1} ]; then
    echo "Job repo is unset. Provide it via first argument like: $(readlink $0)/download_libs_aarch64.sh https://something.com/job/something/{master,stable}"
    exit 1
fi

job_repo=$1

root_dir="$(dirname $(dirname $(readlink $0)))"

curl --location -o ~/Downloads/pkg-ios-aarch64-swift-json.zip $job_repo/aarch64-darwin-ios:lib:simplex-chat.aarch64-darwin/latest/download/1 && \
unzip -o ~/Downloads/pkg-ios-aarch64-swift-json.zip -d ~/Downloads/pkg-ios-aarch64-swift-json

curl --location -o ~/Downloads/pkg-ios-x86_64-swift-json.zip $job_repo/x86_64-darwin-ios:lib:simplex-chat.x86_64-darwin/latest/download/1 && \
unzip -o ~/Downloads/pkg-ios-x86_64-swift-json.zip -d ~/Downloads/pkg-ios-x86_64-swift-json

sh $root_dir/scripts/ios/prepare-x86_64.sh
