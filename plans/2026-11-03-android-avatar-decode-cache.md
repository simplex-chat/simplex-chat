# Android: cache decoded base64 images

## Problem

On Android, scrolling a group's revealed "connected" events stutters.

## Cause

Each revealed event row is from a different member and is displayed with that member's avatar: `MemberImage` -> `MemberProfileImage` -> `ProfileImage` (async = false) -> `base64ToBitmap`, called during composition on the main thread.
On Android, the image is decoded at every `base64ToBitmap` call.
A row scrolled out of view is disposed, and its avatar is decoded again when the row is composed again.

## Fix

On Android, images decoded by `base64ToBitmap` are kept in an `android.util.LruCache`:
- key: the base64 string;
- size: 2 bytes per key character + `Bitmap.byteCount` per entry;
- limit: 1/8 of `Runtime.maxMemory()`;
- error bitmaps and entries larger than the limit are not cached.

## Scope

- `common/src/androidMain/.../platform/Images.android.kt`
- The cache is used by every Android caller of `base64ToBitmap`: avatars (also in calls), image, video and link previews (chat, chat list), video previews in the full-screen viewer, chat-link cards, quotes, compose previews, theme wallpaper, contact-request and call notification icons.
- test: `android/src/androidTest/.../Base64ToBitmapTest.kt`
- `android/build.gradle.kts`: `testInstrumentationRunner` = `androidx.test.runner.AndroidJUnitRunner`.
- `ExampleInstrumentedTest`: the expected package is `BuildConfig.APPLICATION_ID`.
