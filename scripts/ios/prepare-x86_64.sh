#!/bin/sh

set -e

# the binaries folders should be in ~/Downloads folder
rm -rf ./apps/ios/Libraries/mac-aarch64 ./apps/ios/Libraries/mac-x86_64 ./apps/ios/Libraries/ios ./apps/ios/Libraries/sim
mkdir -p ./apps/ios/Libraries/mac-aarch64 ./apps/ios/Libraries/mac-x86_64 ./apps/ios/Libraries/ios ./apps/ios/Libraries/sim
cp ~/Downloads/pkg-ios-aarch64-swift-json/* ./apps/ios/Libraries/mac-aarch64
cp ~/Downloads/pkg-ios-x86_64-swift-json/* ./apps/ios/Libraries/mac-x86_64
chmod +w ./apps/ios/Libraries/mac-aarch64/*
chmod +w ./apps/ios/Libraries/mac-x86_64/*
cp ./apps/ios/Libraries/mac-aarch64/* ./apps/ios/Libraries/ios
cp ./apps/ios/Libraries/mac-x86_64/* ./apps/ios/Libraries/sim
for f in ./apps/ios/Libraries/ios/*; do mac2ios $f; done | wc -l
for f in ./apps/ios/Libraries/sim/*; do mac2ios -s $f; done | wc -l
