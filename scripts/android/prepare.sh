#!/bin/sh

# libsimplex.so and libsupport.so binaries should be in ~/Downloads folder in their directories based on archive name
mkdir -p ./apps/multiplatform/common/src/commonMain/cpp/android/libs/arm64-v8a/
rm ./apps/multiplatform/common/src/commonMain/cpp/android/libs/arm64-v8a/*
unzip -o ~/Downloads/pkg-aarch64-android-libsupport.zip -d ./apps/multiplatform/common/src/commonMain/cpp/android/libs/arm64-v8a/
unzip -o  ~/Downloads/pkg-aarch64-android-libsimplex.zip -d ./apps/multiplatform/common/src/commonMain/cpp/android/libs/arm64-v8a/

mkdir -p ./apps/multiplatform/common/src/commonMain/cpp/android/libs/armeabi-v7a/
rm ./apps/multiplatform/common/src/commonMain/cpp/android/libs/armeabi-v7a/*
unzip -o  ~/Downloads/pkg-armv7a-android-libsupport.zip -d ./apps/multiplatform/common/src/commonMain/cpp/android/libs/armeabi-v7a/
unzip -o  ~/Downloads/pkg-armv7a-android-libsimplex.zip -d ./apps/multiplatform/common/src/commonMain/cpp/android/libs/armeabi-v7a/
