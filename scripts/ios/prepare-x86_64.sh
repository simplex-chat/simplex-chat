#!/bin/sh

set -e

# the binaries folders should be in ~/Downloads folder
rm -rf ./apps/ios/Libraries/mac-aarch64 ./apps/ios/Libraries/mac-x86_64 ./apps/ios/Libraries/ios ./apps/ios/Libraries/sim
mkdir -p ./apps/ios/Libraries/mac-aarch64 ./apps/ios/Libraries/mac-x86_64 ./apps/ios/Libraries/ios ./apps/ios/Libraries/sim
cp ~/Downloads/pkg-ios-aarch64-swift-json/* ./apps/ios/Libraries/mac-aarch64
cp ~/Downloads/pkg-ios-x86_64-swift-json/* ./apps/ios/Libraries/mac-x86_64
chmod +w ./apps/ios/Libraries/mac-aarch64/*
chmod +w ./apps/ios/Libraries/mac-x86_64/*
# project.pbxproj links the core by these names, which don't change with its version
for dir in ./apps/ios/Libraries/mac-aarch64 ./apps/ios/Libraries/mac-x86_64; do
  mv "$dir"/libHSsimplex-chat-*-ghc*.a "$dir/libHSsimplex-chat-ghc.a"
  mv "$dir"/libHSsimplex-chat-[0-9]*.a "$dir/libHSsimplex-chat.a"
done
cp ./apps/ios/Libraries/mac-aarch64/* ./apps/ios/Libraries/ios
cp ./apps/ios/Libraries/mac-x86_64/* ./apps/ios/Libraries/sim
for f in ./apps/ios/Libraries/ios/*; do mac2ios "$f"; done | wc -l
for f in ./apps/ios/Libraries/sim/*; do mac2ios -s "$f"; done | wc -l
