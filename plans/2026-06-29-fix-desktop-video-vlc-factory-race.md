# Fix desktop crash when opening a video (VLC factory init race)

## Problem (user-facing)

Opening a video in full screen on desktop can crash the app with:

```
java.util.NoSuchElementException
  at java.base/java.lang.CompoundEnumeration.nextElement
  ... java.util.ServiceLoader ...
  at uk.co.caprica.vlcj.factory.discovery.provider.DirectoryProviderDiscoveryStrategy.getSupportedProviders
  at uk.co.caprica.vlcj.factory.MediaPlayerFactory.<init>
  at chat.simplex.common.platform.RecAndPlay_desktopKt.vlcFactory_delegate$lambda$0(RecAndPlay.desktop.kt:16)
```

The crash is intermittent and originates from the lazy initialization of the shared
`MediaPlayerFactory` while a video full-screen view is being composed.

## Cause

Each `MediaPlayerFactory()` constructor runs VLC native-library discovery, which iterates a
JDK `ServiceLoader` over `DiscoveryDirectoryProvider`. `ServiceLoader` and the underlying
`CompoundEnumeration` are **not thread-safe**: when two factory constructions run concurrently
on different threads, one enumeration reports `hasNext() == true` and then throws
`NoSuchElementException` from `nextElement()`.

There are two factories on the desktop:

- `vlcFactory` — used by the real audio/video players. Its lazy init is triggered on the
  AWT/Compose render thread when a video is opened full screen
  (`VideoPlayer.initializeMediaPlayerComponent` -> `RecAndPlay.desktop.kt:16`).
- `vlcPreviewFactory` (`--avcodec-hw=none`) — used by preview snapshot helpers, whose lazy init
  runs on the dedicated `previewThread` (`VideoPlayer.getOrCreateHelperPlayer`).

The single-factory invariant established by #6739 ("use shared VLC media-player factory") was
the original protection against concurrent factory construction. #6924 reintroduced a second
factory (`vlcPreviewFactory`) for hardware-acceleration-free previews, reopening the race: the
render thread can construct `vlcFactory` while `previewThread` constructs `vlcPreviewFactory`,
producing two concurrent `ServiceLoader` discoveries and the crash.

## Fix

Serialize the two `MediaPlayerFactory()` constructions behind a shared lock so their
native-discovery / `ServiceLoader` runs can never overlap:

```kotlin
private val vlcFactoryLock = Any()
internal val vlcFactory: MediaPlayerFactory by lazy { synchronized(vlcFactoryLock) { MediaPlayerFactory() } }
internal val vlcPreviewFactory: MediaPlayerFactory by lazy { synchronized(vlcFactoryLock) { MediaPlayerFactory("--avcodec-hw=none") } }
```

Both factories are preserved, including the preview factory's `--avcodec-hw=none` option. The
lock guards only the one-time construction of each factory, so there is no steady-state
contention once both are built.

### Why this approach

- **Minimal and intent-preserving.** Keeps both factories (preview still needs
  `--avcodec-hw=none`) and only adds serialization, restoring the no-concurrent-construction
  guarantee that #6739 relied on.
- **Lazy-preserving.** Each factory is still built strictly on demand; the lock only matters in
  the rare window where both initialize at the same time. A smaller diff (forcing
  `vlcFactory` first inside the preview initializer) was rejected because it would eagerly
  construct the main factory whenever a preview is generated and reads as dead code.

### Known trade-off

Because `vlcFactory` is initialized on the AWT/render thread, if `previewThread` is mid-construction
of `vlcPreviewFactory` the render thread can briefly block on the lock until native discovery
finishes. This replaces an intermittent crash with a rare, short stall — an acceptable trade.
A more thorough follow-up would either collapse to a single factory (passing `:avcodec-hw=none`
as a per-media option on preview prepare) or eagerly initialize both factories off the render
thread at startup.

## Scope

- `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/platform/RecAndPlay.desktop.kt`
