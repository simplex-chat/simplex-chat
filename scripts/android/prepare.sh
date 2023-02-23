#!/bin/sh

# libsimplex.so and libsupport.so binaries should be in ~/Downloads folder
mkdir -p ./apps/android/app/src/main/cpp/libs/arm64-v8a/
rm ./apps/android/app/src/main/cpp/libs/arm64-v8a/*
cp ~/Downloads/libsupport.so ./apps/android/app/src/main/cpp/libs/arm64-v8a/
cp ~/Downloads/pkg-aarch64-android-libsimplex/libsimplex.so ./apps/android/app/src/main/cpp/libs/arm64-v8a/
cp ~/Downloads/pkg-aarch64-android-libsimplex/libcrypto.so ./apps/android/app/src/main/cpp/libs/arm64-v8a/
