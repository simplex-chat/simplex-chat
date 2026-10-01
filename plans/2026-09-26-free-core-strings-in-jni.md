# Free strings returned by the core in the JNI bindings

## Problem

On desktop and Android, resident memory grows for as long as the app runs and is
only released by restarting it. On a profile with ~200k connections it reaches
several GB within a day; the growth continues while the app is idle.

None of it is visible to either heap tool. The JVM heap stays small, and the
Haskell heap accounts for its own region only — the memory is in the C allocator,
which neither profiler walks.

## Cause

Every `chat_*` entry point returns a string that the core allocates with `malloc`
and hands to the caller. `Simplex.Chat.Mobile.Shared` is explicit about it:

```haskell
newCStringFromLazyBS :: LB.ByteString -> IO CString
newCStringFromLazyBS s = do
  ...
  buf <- mallocBytes (len + 1)
```

The caller owns that buffer. iOS honours the contract — `SimpleXChat/API.swift`
calls `free(c)` after copying each response into a Swift string. The two JNI
bindings never do.

Both of them copy the bytes into a Java string and then drop the pointer:

- desktop `decode_to_utf8_string` wraps the buffer in a `DirectByteBuffer`,
  decodes it through `Charset.decode`, returns the resulting `String`, and
  deletes only the JNI local references;
- Android calls `NewStringUTF(env, chat_*())` inline at every entry point, so the
  pointer is never even bound to a variable.

`chatReadFile` leaks the same way on both platforms, after `SetByteArrayRegion`
has already copied the payload.

So every command, every received event, and every file read leaks its whole
response. Nothing bounds it: the buffers are unreachable from both runtimes, the
Java string is a copy, and no code path retains the original.

### Measurements

A direct-FFI harness calling `chat_send_cmd_retry` in a loop, outside the JVM,
separates the two cases cleanly:

| harness | behaviour |
|---|---|
| response pointer dropped (current bindings) | +5 KB per call, linear, no plateau |
| response pointer freed | rises to 138 MB and stays flat |

Per interface event the mean response is ~3.9 KB, so the rate follows activity
rather than connection count.

On a desktop client running the affected profile, `/proc/<pid>/smaps` attributes
roughly 1.0 GB to glibc's non-main arenas — sixteen mappings of 63-64 MB, which is
`HEAP_MAX_SIZE` on x86-64, aligned to 64 MB boundaries. Those pages are counted as
touched, not merely reserved. Over the ~10 hours of uptime that is on the order of
100 MB/hour, against a Haskell heap of 1.70 GB and a JVM heap of 328 MB in the
same process.

## Fix

Free each buffer once its contents have been copied.

Desktop already has a single funnel, `decode_to_utf8_string`, which every entry
point goes through, so one `free` covers all of them. Android has no such funnel
because `NewStringUTF` is called inline, so the fix adds `decode_and_free` and
routes the sixteen call sites through it. `chatReadFile` frees its buffer directly
on both platforms.

Android cannot include `stdlib.h` — the file defines a `reallocarray` stub that
conflicts with it — so `free` is declared on its own.

The change is confined to the two binding files and does not alter what either
function returns.

### Why this is safe

In every case the copy is complete before the buffer is freed:

- `Charset.decode` produces a `CharBuffer` whose backing array is freshly
  allocated, and `toString` copies again into the `String`;
- `NewStringUTF` copies into a new Java string;
- `SetByteArrayRegion` copies into the `byte[]` before either `free` runs.

Nothing retains the original pointer. On desktop the `DirectByteBuffer` is a view
over it, but it is a JNI local reference deleted in the same function and never
escapes to Java.

Note that the *input* direction was already correct by accident: the bindings pass
the `malloc`ed buffers from `encode_to_utf8_chars` to `ReleaseStringUTFChars`,
which is not the matching deallocator but reaches `free` through HotSpot's
`FreeHeap`. That mismatch is left alone here.

## Verification

- Desktop AppImage with the fix, on a fresh profile: flat at 806-808 MB RSS
  through 35 minutes of traffic followed by 10 minutes idle.
- Both `free` calls confirmed in the shipped binary by disassembling
  `libapp-lib.so` out of the AppImage.
