# Android: cache decoded base64 images

## Problem

On Android, scrolling a group's revealed "connected" events stutters.

## Cause

Each revealed event row shows the avatar of another member: `MemberImage` -> `MemberProfileImage` -> `ProfileImage` (async = false) -> `base64ToBitmap`, called during composition on the main thread.
Android `base64ToBitmap` decodes the image at every call.
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
- Android callers of `base64ToBitmap` read the cache: avatars (also in calls), image, video and link previews (chat, chat list, full-screen viewer), chat-link cards, quotes, compose previews, theme wallpaper, message and call notification icons.
- test: `android/src/androidTest/.../Base64ToBitmapTest.kt`
- `android/build.gradle.kts`: `testInstrumentationRunner` is `androidx.test.runner.AndroidJUnitRunner`, the runner the test APK contains.
