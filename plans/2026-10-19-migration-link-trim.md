# Migration: a pasted link with leading (or trailing non-ASCII) whitespace drops the link's network settings

## Problem

"Migrate here" → "Tap to paste link" (and, on Android, scanning the QR code) passes the text to `checkUserLink` (`MigrateToDevice.kt`). It checks the trimmed text, but reads the link's network settings from the raw text:

```kotlin
return if (strHasSimplexFileLink(link.trim())) {
  val data = MigrationFileLinkData.readFromLink(link)
```

`readFromLink` sends `/_download info <link>` to the core. Before parsing, the core strips trailing bytes that Haskell's `isSpace` matches (`parseChatCommand`, `Commands.hs`: ASCII whitespace and byte 0xA0). A leading space or newline makes the parse fail; a trailing non-ASCII whitespace character that Kotlin's `trim()` removes (e.g. a no-break space, U+00A0, bytes C2 A0) leaves stray bytes in the link's `data=` parameter, so the request returns no network settings. Either way `readFromLink` returns null. With no data:

- the "Confirm network settings" step is skipped;
- the archive downloads with this device's settings and no SOCKS proxy, even when the source device put its proxy (e.g. Tor) in the link;
- after migration the new device has "Use SOCKS proxy" off.

The download itself works, because the state payloads already use the trimmed link, so nothing tells the user. A silent privacy downgrade for a user migrating from a Tor setup.

Android and desktop share this code. iOS does not read network settings from the link at all (its `readFromLink` is a stub returning nil), so this bug cannot happen there (iOS has a different, visible issue: a link with a leading newline is rejected as invalid — separate).

## Fix

Trim once at the top of `checkUserLink` and use the trimmed value for the check, for `readFromLink` and for the state payloads (which already used `link.trim()`).

## Alternatives considered

- `readFromLink(link.trim())` only: same effect, but keeps five separate `trim()` calls in a few lines — the inconsistency that caused this bug.
- Trim inside `readFromLink` or at the callers: works, but splits the normalisation across places.
- Make the core parser accept leading whitespace: changes parsing of every command (or, if targeted at `/_download info`, still needs a core release) for a client-side mistake.
- Report a failed network-settings read instead of skipping it: a link that fails to parse already comes back as an error, but stray bytes in `data=` give the same "no network settings" as a valid link without them, so telling them apart needs a new result type; after this fix a link the core can't parse fails the download visibly. (Trailing non-whitespace picked up when copying — e.g. a "." or a zero-width space — still ends up in `data=` and silently drops the settings; out of this PR's whitespace scope.)

## Scope

All other paste paths that classify a link and then use it pass the parsed, clean value (connect-via-link paste, chat-list search, new-chat sheet; the Android connect scan checks and uses the same raw string), or use the same raw string and show a visible error (server addresses, desktop address). Only this function had the silent split.

## Testing

Desktop, master vs this PR: a source instance with "Use SOCKS proxy" on creates a migration link; the target pastes it with leading spaces, a leading newline, or a trailing no-break space (master: "Confirm network settings" skipped, and — after a full migration with two leading spaces — proxy off; PR: the settings screen is shown, proxy on). Regression: an unpadded link, a trailing newline only, padded non-link text ("Invalid link"), a normal migration.
