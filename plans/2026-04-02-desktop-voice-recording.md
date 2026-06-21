# Desktop Voice Recording

## Overview

Implement voice recording on desktop using vlcj (already a dependency). The `RecorderNative` class is currently a stub. All UI is already in common code.

## Files to modify

1. `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/platform/RecAndPlay.desktop.kt` — implement `RecorderNative`
2. `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/SendMsgView.kt` — remove desktop "in development" guard (line 317-318)
3. `apps/multiplatform/desktop/build.gradle.kts` — add `NSMicrophoneUsageDescription` to macOS Info.plist

## RecorderNative implementation

Uses `MediaPlayerFactory` + `MediaPlayer` to capture from default microphone and transcode to AAC/m4a via VLC's sout chain.

Platform-specific capture MRLs:
- macOS: `qtsound://`
- Linux: `pulse://`
- Windows: `dshow://` with `:dshow-vdev=none :dshow-adev=`

Transcode options: `vcodec=none,acodec=mp4a,ab=32,channels=1,samplerate=16000` — matches Android (mono, 16kHz, 32kbps AAC).

Factory requires `--sout-avcodec-strict=-2` to enable FFmpeg's native AAC encoder.

Progress tracked via elapsed time (VLC capture has no position API). Duration read via `AudioPlayer.duration()` after stop.

Max duration: enforced by stopping recording after `MAX_VOICE_MILLIS_FOR_SENDING` (300,000 ms) in the progress coroutine.

## macOS permission

Add `NSMicrophoneUsageDescription` to Info.plist via Gradle `infoPlist` block.

## What does NOT change

- `RecorderInterface` (common)
- `ComposeView.kt`, `ComposeVoiceView` — already handle voice preview/sending
- Audio format — `.m4a` (matches Android)
- All voice recording UI — already in common code
