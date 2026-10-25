# Migration: a pasted link with leading whitespace, or trailing whitespace the core does not strip, drops the link's network settings

## Problem

"Migrate here" → "Tap to paste link" (and, on Android, scanning the QR code) passes the text to `checkUserLink` (`MigrateToDevice.kt`). It checks the trimmed text, but reads the link's network settings from the raw text:

```kotlin
return if (strHasSimplexFileLink(link.trim())) {
  val data = MigrationFileLinkData.readFromLink(link)
```

`readFromLink` sends `/_download info <link>` to the core. Before parsing, the core strips trailing bytes that Haskell's `isSpace` matches (`parseChatCommand`, `Commands.hs`: bytes 0x09-0x0D, 0x20 and 0xA0). A leading space or newline makes the parse fail; a trailing whitespace character that Kotlin's `trim()` removes but the core does not strip (non-ASCII, e.g. a no-break space, U+00A0, bytes C2 A0, or ASCII U+001C-U+001F) leaves stray bytes in the link's `data=` parameter, so the request returns no network settings. Either way `readFromLink` returns null. With no data:

- the "Confirm network settings" step is skipped;
- the archive downloads with no SOCKS proxy at all (`socksProxy = null`): the proxy the source device put in the link (e.g. Tor) is not used, and neither is a proxy set on this device, so the XFTP servers receive connections from the device's network address (without a VPN, its public IP) during the download;
- after migration the new device has "Use SOCKS proxy" off.

The download itself works, because the state payloads already use the trimmed link, so no error is shown. A silent privacy downgrade for a user migrating from a Tor setup.

Android and desktop share this code. iOS does not read network settings from the link at all (its `readFromLink` is a stub returning nil), so this bug cannot happen there (iOS has a different, visible issue: a link with a leading newline is rejected as invalid — separate).

## Fix

Trim once at the top of `checkUserLink` and use the trimmed value for the check, for `readFromLink` and for the state payloads (which already used `link.trim()`).

## Alternatives considered

- `readFromLink(link.trim())` only: same effect, but adds a fifth `trim()` call to a function that already has four; the bug was the one place where the trim was missed, and one trimmed value used by the check, the read and the state payloads replaces that repetition.
- Shadow the parameter (`val link = link.trim()`): also makes the untrimmed text unreachable, which the chosen fix does not, and the project already uses this pattern (e.g. `val state = state ?: handler.listState`, `ScrollableColumn.desktop.kt:65`). Rejected because the line with the bug (`readFromLink(link)`) would not change, so the diff would not show the fix.
- Trim inside `readFromLink` or at the callers: works, but splits the normalisation across places.
- Make the core parser accept leading whitespace: changes parsing of every command (or, if targeted at `/_download info`, still needs a core release) for a client-side mistake, and does not fix the trailing no-break space, whose stray byte stays in `data=`.
- Report a failed network-settings read instead of skipping it: this makes the failure visible, but a padded link would still not work, so the user would have to paste it again; trimming makes it work. Reporting would make the triggers outside this PR's edge-whitespace scope visible; left for a follow-up:
  - Text after the link, after a space or a newline (e.g. `LINK sent` or `LINK` followed by a blank line and "Sent from my phone"): `/_download info` fails to parse, but `/_download` still parses, because the link ends at the space or newline and the rest of the text becomes part of the archive path. So the settings are skipped and the archive download starts without any proxy. The core already returns an error for this, distinct from "no settings", but `standaloneFileInfo` returns null for both, so reporting it needs a change to what `standaloneFileInfo` returns, but no core change.
  - Trailing non-whitespace picked up when copying (e.g. a "." or a zero-width space): it ends up in `data=`, the settings cannot be decoded, and the core returns no settings (`fileMeta = null`). Both apps always write `data` with network settings into the link, so a readable link always returns settings, and the client can distinguish this case without a core change (again by changing what `standaloneFileInfo` returns, since it returns null here too).
  - Trailing Unicode whitespace that Kotlin's `trim()` does not remove, e.g. U+0085 (bytes C2 85, which the core does not strip either): same as trailing non-whitespace.

## Scope

All other paste paths that classify a link and then use it pass the parsed, clean value (connect-via-link paste, chat-list search, new-chat sheet), or check and use the same raw string (the Android connect scan; server addresses and desktop address, which show a visible error). Only this function had the silent split.

## Testing

Desktop, master vs this PR: a source instance with "Use SOCKS proxy" on creates a migration link; the target pastes it with leading spaces, a leading newline, or a trailing no-break space (master: "Confirm network settings" skipped, and — after a full migration with two leading spaces — proxy off; PR: the settings screen is shown, proxy on). Regression: an unpadded link, a trailing newline only, padded non-link text ("Invalid link"), a normal migration.
