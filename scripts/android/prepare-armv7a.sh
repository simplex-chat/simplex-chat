#!/bin/sh

# libsimplex.so and libsupport.so binaries should be in ~/Downloads folder
mkdir -p ./apps/android/app/src/main/cpp/libs/armeabi-v7a
rm ./apps/android/app/src/main/cpp/libs/armeabi-v7a/*
cp ~/Downloads/libsupport.so ./apps/android/app/src/main/cpp/libs/armeabi-v7a/
cp ~/Downloads/pkg-armv7a-android-libsimplex/libsimplex.so ./apps/android/app/src/main/cpp/libs/armeabi-v7a/
cp ~/Downloads/pkg-armv7a-android-libsimplex/libcrypto.so ./apps/android/app/src/main/cpp/libs/armeabi-v7a/
