# Android: cache decoded base64 images

## Problem

On Android, scrolling a group's revealed "connected" events stutters.

## Cause

Each revealed event row shows the avatar of another member: `MemberImage` -> `ProfileImage` (async = false) -> `base64ToBitmap`.
Android `base64ToBitmap` decodes the JPEG on the main thread at every call.
A row that scrolls off is disposed, and its avatar is decoded again when it returns.
Desktop `base64ToBitmap` keeps decoded images in an LRU cache (#6631); Android has none.

## Fix

Android `base64ToBitmap` keeps decoded images in an `android.util.LruCache`:
- key: the base64 string;
- size: `Bitmap.byteCount` per entry;
- limit: 1/8 of `Runtime.maxMemory()`;
- error bitmaps are not cached.

## Scope

- `common/src/androidMain/.../platform/Images.android.kt`
- Android callers of `base64ToBitmap` read the cache: avatars, image, video and link previews, quotes, compose previews, theme wallpaper.
- test: `android/src/androidTest/.../Base64ToBitmapTest.kt`
- `android/build.gradle.kts`: `testInstrumentationRunner` is `androidx.test.runner.AndroidJUnitRunner`, the runner the test APK contains.
