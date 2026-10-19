# Fix Android app opening simplex.chat web pages as connection links

## Problem

Tapping `https://simplex.chat/crowdfunding/` in a chat and confirming "Open" in the
"Open web link?" alert shows "Invalid connection link" instead of opening the page
in the browser. `/call/`, `/credits/` and `/about/` are affected the same way. iOS and
desktop are not.

## Cause

"Open" calls `safeOpenUri` → `uriHandler.openUri` (`TextItemView.kt`), which on Android
fires an `ACTION_VIEW` intent. The app's own verified app-link filter in
`android/src/main/AndroidManifest.xml` matches the `simplex.chat` host with:

```xml
<data android:pathPrefix="/invitation" />
<data android:pathPrefix="/contact" />
<data android:pathPrefix="/a" />
<data android:pathPrefix="/c" />
<data android:pathPrefix="/g" />
<data android:pathPrefix="/i" />
```

`pathPrefix` is a plain string prefix, so `/c` also matches `/crowdfunding/`, `/call/`
and `/credits/`, and `/a` matches `/about/`. The single-letter prefixes were added
for short links in #5824. Android delivers the intent back to SimpleX, `processIntent`
sets `appOpenUrl`, and `connectIfOpenedViaUri` → `planAndConnect` tries to connect via
the web page URL and fails.

iOS is not affected: `website/src/.well-known/apple-app-site-association` lists exact
components (`/c` and `/c/*`, etc.), not string prefixes. Desktop registers no https
handler.

## Fix

Make the Android filter match the iOS one: replace each `pathPrefix="/X"` with an exact
`path="/X"` plus `pathPrefix="/X/"`.

```xml
<data android:path="/c" />
<data android:pathPrefix="/c/" />
```

Connection links carry their data in the fragment (`/contact#/?v=…`, `/a#key`), which
is not part of the matched path, so they still match the exact path. Links with a
trailing slash or a subpath (`/contact/#…`, `/c/…`) match the `/X/` prefix, the same as
`/X/*` on iOS.

App-link verification (`autoVerify`, `assetlinks.json`) is per host and is not changed
by the path list.

## Alternatives considered

- Detecting non-connection URLs in `processIntent` and re-opening them in the browser:
  a second `ACTION_VIEW` resolves back to the same filter, so the app would need an
  explicit browser component. That still sends every such link through SimpleX first,
  including links tapped in other apps.
- `pathPattern`: it has no anchors or alternation that `path` + `pathPrefix` doesn't
  already cover, and it is harder to read.

## Verification

Checked on an API 27 emulator using stub APKs that contain only the app's intent
filter, with `cmd package query-activities -a android.intent.action.VIEW -c
android.intent.category.BROWSABLE -d <url>`:

| URL | before | after |
|---|---|---|
| `https://simplex.chat/crowdfunding/` | SimpleX + browser | browser |
| `https://simplex.chat/call/` | SimpleX + browser | browser |
| `https://simplex.chat/credits/` | SimpleX + browser | browser |
| `https://simplex.chat/about/` | SimpleX + browser | browser |
| `https://simplex.chat/contacts` | SimpleX + browser | browser |
| `https://simplex.chat/blog/` | browser | browser |
| `https://simplex.chat/contact#/?v=1` | SimpleX + browser | SimpleX + browser |
| `https://simplex.chat/contact/#/?v=1` | SimpleX + browser | SimpleX + browser |
| `https://simplex.chat/invitation#/?v=1` | SimpleX + browser | SimpleX + browser |
| `https://smp4.simplex.im/a#abc` | SimpleX + browser | SimpleX + browser |
| `https://simplex.chat/c#abc` | SimpleX + browser | SimpleX + browser |
| `https://simplex.chat/c/abc` | SimpleX + browser | SimpleX + browser |
| `https://simplex.chat/g#x` | SimpleX + browser | SimpleX + browser |
| `https://simplex.chat/i#x` | SimpleX + browser | SimpleX + browser |

The built arm64 debug APK's manifest was dumped with `aapt2` and contains the new
`path` / `pathPrefix` entries.
