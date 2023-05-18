#!/bin/sh

set -e

# the binaries folder should be in ~/Downloads folder
rm -rf ./apps/ios/Libraries/mac ./apps/ios/Libraries/ios ./apps/ios/Libraries/sim
mkdir -p ./apps/ios/Libraries/mac ./apps/ios/Libraries/ios ./apps/ios/Libraries/sim
unzip -o ~/Downloads/pkg-ios-aarch64-swift-json.zip -d ./apps/ios/Libraries/mac
chmod +w ./apps/ios/Libraries/mac/*
cp ./apps/ios/Libraries/mac/* ./apps/ios/Libraries/ios
cp ./apps/ios/Libraries/mac/* ./apps/ios/Libraries/sim
for f in ./apps/ios/Libraries/ios/*; do mac2ios $f; done | wc -l
for f in ./apps/ios/Libraries/sim/*; do mac2ios -s $f; done | wc -l
