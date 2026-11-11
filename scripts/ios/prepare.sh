#!/bin/sh

set -e

# the binaries folder should be in ~/Downloads folder
rm -rf ./apps/ios/Libraries/mac ./apps/ios/Libraries/ios ./apps/ios/Libraries/sim
mkdir -p ./apps/ios/Libraries/mac ./apps/ios/Libraries/ios ./apps/ios/Libraries/sim
unzip -o ~/Downloads/pkg-ios-aarch64-swift-json.zip -d ./apps/ios/Libraries/mac
chmod +w ./apps/ios/Libraries/mac/*
# project.pbxproj links the core by these names, which don't change with its version
mv ./apps/ios/Libraries/mac/libHSsimplex-chat-*-ghc*.a ./apps/ios/Libraries/mac/libHSsimplex-chat-ghc.a
mv ./apps/ios/Libraries/mac/libHSsimplex-chat-[0-9]*.a ./apps/ios/Libraries/mac/libHSsimplex-chat.a
cp ./apps/ios/Libraries/mac/* ./apps/ios/Libraries/ios
cp ./apps/ios/Libraries/mac/* ./apps/ios/Libraries/sim
for f in ./apps/ios/Libraries/ios/*; do mac2ios "$f"; done | wc -l
for f in ./apps/ios/Libraries/sim/*; do mac2ios -s "$f"; done | wc -l
