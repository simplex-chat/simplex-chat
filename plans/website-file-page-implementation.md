# File Transfer Page — Implementation Plan

## Table of Contents
1. [Context](#1-context)
2. [Executive Summary](#2-executive-summary)
3. [High-Level Design](#3-high-level-design)
4. [Detailed Implementation Plan](#4-detailed-implementation-plan)
5. [Known Divergences from Product Plan](#5-known-divergences-from-product-plan)
6. [Verification](#6-verification)

---

## 1. Context

**Problem**: The website needs a `/file` page that lets users upload/download files via XFTP servers directly in the browser — a live demo that funnels users toward downloading the SimpleX app.

**Product plan**: `plans/website-file-page-product.md`

**Approach**: Use the pre-built `dist-web/` bundle from `@shhhum/xftp-web@0.8.0`. Copy three files (`index.js` + `index.css` + `crypto.worker.js`) to website static assets. Wrap with an 11ty page providing the protocol overlay, app download CTA, and i18n bridge. **No Vite/TS build step.** The bundle handles all XFTP protocol, crypto, Web Worker, upload/download UI.

**Library features used** (v0.8.0):
- `data-xftp-app` — configurable target element
- `data-no-hashchange` — prevents conflict with overlay system
- `window.__XFTP_I18N__` — string externalization for i18n
- `xftp:upload-complete` / `xftp:download-complete` — CustomEvents for CTA injection
- Scoped CSS (`#app` / `.dark #app`) — no global resets
- Relative worker URL — both files co-located in same directory

**Routing**: `/file/` (no hash) = upload mode; `/file/#<uri>` = download mode.

---

## 2. Executive Summary

| Action | Files |
|--------|-------|
| **Create** | `website/src/file.html`, `website/src/_data/file_overlays.json`, `website/src/_includes/overlay_content/file/protocol.html` |
| **Copy from npm** | `dist-web/assets/index.js` + `dist-web/assets/index.css` + `dist-web/assets/crypto.worker.js` → `src/file-assets/` |
| **Modify** | `website/package.json`, `website/.eleventy.js`, `website/src/_includes/navbar.html`, `website/langs/en.json` (~30 keys), `website/web.sh`, `website/src/js/script.js`, `.gitignore` |

---

## 3. High-Level Design

### Architecture

```
website/src/
├── file.html                           # 11ty page
├── _data/file_overlays.json            # overlay config (showImage: false for v1)
├── _includes/overlay_content/file/
│   └── protocol.html                   # protocol popup content
└── file-assets/                        # COPIED from npm dist-web/assets/ (gitignored)
    ├── index.js                        # main bundle (~1.1 MB)
    ├── index.css                       # scoped CSS (~2.3 KB)
    └── crypto.worker.js               # worker (~1.0 MB)
```

### Data flow

**Upload**: `#app` div → bundle renders drop zone → file input → Worker encrypts (OPFS) → `uploadFile()` → share link → `xftp:upload-complete` event → website shows inline CTA

**Download**: hash parsed by bundle on init → `decodeDescriptionURI()` → download button → Worker decrypts → browser save → `xftp:download-complete` event → website shows inline CTA

### Overlay conflict resolution

Bundle's `hashchange` listener is disabled via `data-no-hashchange` attribute. Protocol overlay opens via **direct DOM manipulation** (inline JS `classList.remove('hidden')`) — not hash-based. script.js's global `.close-overlay-btn` handler still closes it. No hash events fired when opening.

Note: `closeOverlay()` in script.js calls `history.replaceState(null, null, ' ')` which clears the URL hash. In download mode (`/file/#simplex:...`), this means the hash disappears from the URL bar after closing the overlay. This is cosmetic only — the bundle parses the hash once on init and doesn't re-read it. Download continues unaffected.

A null guard is added to `openOverlay()` in script.js (Step 9) to prevent crashes when the hash is an XFTP URI fragment rather than a DOM element ID.

### i18n bridge

The 11ty template renders `window.__XFTP_I18N__` from en.json keys. The bundle reads via `t(key, fallback)`. All JS-rendered strings are overridable. The bundle renders strings via template literals into innerHTML, so HTML in i18n values (e.g. links in `maxSizeHint`) is rendered correctly.

---

## 4. Detailed Implementation Plan

### Step 1: Add npm dependency

**Modify**: `website/package.json`

```diff
 "dependencies": {
+  "@shhhum/xftp-web": "^0.8.0",
 }
```

### Step 2: Copy dist-web files in web.sh

**Modify**: `website/web.sh`

After the existing `cp node_modules/...` lines (after line 30):

```bash
mkdir -p src/file-assets
cp node_modules/@shhhum/xftp-web/dist-web/assets/index.js src/file-assets/
cp node_modules/@shhhum/xftp-web/dist-web/assets/index.css src/file-assets/
cp node_modules/@shhhum/xftp-web/dist-web/assets/crypto.worker.js src/file-assets/
```

Add `file.html` to language copy loop (after line 42, `cp src/fdroid.html src/$lang`):
```bash
  cp src/file.html src/$lang
```

### Step 3: Create 11ty page — `website/src/file.html`

```
---
layout: layouts/main.html
title: "SimpleX File Transfer"
description: "Send files securely with end-to-end encryption"
templateEngineOverride: njk
active_file: true
---
{% set lang = page.url | getlang %}
{% from "components/macro.njk" import overlay %}
```

**Structure** (top to bottom):

1. **Noscript fallback**:
   ```html
   <noscript>
     <p class="text-center text-grey-black dark:text-white py-10">
       {{ "file-noscript" | i18n({}, lang) | safe }}
     </p>
   </noscript>
   ```

2. **Page section** with centered container:
   - `<h1>` with i18n title
   - `<div id="app" data-xftp-app data-no-hashchange>` — bundle renders here, hashchange disabled
   - Static "E2E encrypted" note below `#app`:
     ```html
     <p class="text-center text-sm text-gray-500 dark:text-gray-400 mt-3">
       {{ "file-e2e-note" | i18n({}, lang) | safe }}
     </p>
     ```
   - "Learn more" link (opens overlay via inline JS, not hash):
     ```html
     <p class="text-center mt-4">
       <a id="learn-more-btn" href="javascript:void(0);" class="text-active-blue hover:underline">
         {{ "file-learn-more" | i18n({}, lang) | safe }}
       </a>
     </p>
     ```

3. **Inline CTA container** (hidden, shown by JS after upload/download):
   ```html
   <div id="inline-cta" class="hidden text-center py-8">
     <h2 class="text-2xl font-bold text-grey-black dark:text-white mb-3">
       {{ "file-cta-heading" | i18n({}, lang) | safe }}
     </h2>
     <p class="text-base text-grey-black dark:text-white mb-6 max-w-[600px] mx-auto">
       {{ "file-cta-subheading" | i18n({}, lang) | safe }}
     </p>
     <div class="flex items-center justify-center gap-4 flex-wrap">
       <!-- Same 5 store buttons as join_simplex.html -->
       <a href="https://apps.apple.com/us/app/simplex-chat/id1605771084" target="_blank"><img class="h-[40px] w-auto" src="/img/new/apple_store.svg" /></a>
       <a href="https://play.google.com/store/apps/details?id=chat.simplex.app" target="_blank"><img class="h-[40px] w-auto" src="/img/new/google_play.svg" /></a>
       <a href="{{ '' if lang == 'en' else '/' ~ lang }}/fdroid"><img class="h-[40px] w-auto" src="/img/new/f_droid.svg" /></a>
       <a href="https://testflight.apple.com/join/DWuT2LQu" target="_blank"><img class="h-[40px] w-auto" src="/img/new/testflight.png" /></a>
       <a href="https://github.com/simplex-chat/simplex-chat/releases/latest/download/simplex-aarch64.apk" target="_blank"><img class="h-[40px] w-auto" src="/img/new/apk_icon.png" /></a>
     </div>
   </div>
   ```

4. **Protocol overlay** via existing macro:
   ```html
   {% for section in file_overlays.sections %}
     {{ overlay(section, lang) }}
   {% endfor %}
   ```

5. **Bottom CTA section** (same pattern as `join_simplex.html`):
   - Heading: "Get SimpleX — the most private messenger"
   - Subheading about the app using the same protocol
   - 5 buttons: Apple Store, Google Play, F-Droid, TestFlight, APK (same markup as inline CTA)

6. **i18n bridge script** (BEFORE bundle load, so `window.__XFTP_I18N__` is set when bundle initializes):
   ```html
   <script>
     window.__XFTP_I18N__ = {
       "title": "{{ 'file-title' | i18n({}, lang) | safe }}",
       "dropZone": "{{ 'file-drop-text' | i18n({}, lang) | safe }}",
       "dropZoneHint": "{{ 'file-drop-hint' | i18n({}, lang) | safe }}",
       "chooseFile": "{{ 'file-choose' | i18n({}, lang) | safe }}",
       "maxSizeHint": "{{ 'file-max-size' | i18n({}, lang) | safe }}",
       "encrypting": "{{ 'file-encrypting' | i18n({}, lang) | safe }}",
       "uploading": "{{ 'file-uploading' | i18n({}, lang) | safe }}",
       "cancel": "{{ 'file-cancel' | i18n({}, lang) | safe }}",
       "fileUploaded": "{{ 'file-uploaded' | i18n({}, lang) | safe }}",
       "copy": "{{ 'file-copy' | i18n({}, lang) | safe }}",
       "copied": "{{ 'file-copied' | i18n({}, lang) | safe }}",
       "share": "{{ 'file-share' | i18n({}, lang) | safe }}",
       "expiryHint": "{{ 'file-expiry' | i18n({}, lang) | safe }}",
       "securityNote1": "{{ 'file-sec-1' | i18n({}, lang) | safe }}",
       "securityNote2": "{{ 'file-sec-2' | i18n({}, lang) | safe }}",
       "securityNote3": "{{ 'file-sec-3' | i18n({}, lang) | safe }}",
       "retry": "{{ 'file-retry' | i18n({}, lang) | safe }}",
       "downloading": "{{ 'file-downloading' | i18n({}, lang) | safe }}",
       "decrypting": "{{ 'file-decrypting' | i18n({}, lang) | safe }}",
       "downloadComplete": "{{ 'file-download-complete' | i18n({}, lang) | safe }}",
       "download": "{{ 'file-download-btn' | i18n({}, lang) | safe }}",
       "fileTooLarge": "{{ 'file-too-large' | i18n({}, lang) | safe }}",
       "fileEmpty": "{{ 'file-empty' | i18n({}, lang) | safe }}",
       "invalidLink": "{{ 'file-invalid-link' | i18n({}, lang) | safe }}",
       "initError": "{{ 'file-init-error' | i18n({}, lang) | safe }}",
       "fileAvailable": "{{ 'file-available' | i18n({}, lang) | safe }}",
       "dlSecurityNote1": "{{ 'file-dl-sec-1' | i18n({}, lang) | safe }}",
       "dlSecurityNote2": "{{ 'file-dl-sec-2' | i18n({}, lang) | safe }}",
       "dlSecurityNote3": "{{ 'file-dl-sec-3' | i18n({}, lang) | safe }}",
       "workersRequired": "{{ 'file-workers-required' | i18n({}, lang) | safe }}"
     }
   </script>
   ```

7. **Overlay open + CTA injection script**:
   ```html
   <script>
     document.getElementById('learn-more-btn')?.addEventListener('click', function() {
       var el = document.getElementById('xftp-protocol');
       if (el) { el.classList.remove('hidden'); el.classList.add('flex'); document.body.classList.add('lock-scroll'); }
     });
     document.getElementById('app')?.addEventListener('xftp:upload-complete', function() {
       document.getElementById('inline-cta')?.classList.remove('hidden');
     });
     document.getElementById('app')?.addEventListener('xftp:download-complete', function() {
       document.getElementById('inline-cta')?.classList.remove('hidden');
     });
   </script>
   ```

8. **Bundle + CSS** (bundle AFTER i18n bridge):
   ```html
   <link rel="stylesheet" href="/file-assets/index.css">
   <script type="module" src="/file-assets/index.js"></script>
   ```

### Step 4: Create protocol overlay data + content

**New file**: `website/src/_data/file_overlays.json`
```json
{
  "sections": [{
    "id": 1,
    "imgLight": "",
    "imgDark": "",
    "overlayContent": {
      "overlayId": "xftp-protocol",
      "overlayScrollTo": "",
      "title": "file-protocol-title",
      "showImage": false,
      "contentBody": "overlay_content/file/protocol.html"
    }
  }]
}
```

Note: `showImage: false` — protocol diagram SVGs are deferred to a future iteration. The overlay works without images (same as existing overlays when `showImage` is false — the content section spans full width).

**New file**: `website/src/_includes/overlay_content/file/protocol.html`

5 blocks with heading + paragraph structure (existing hero overlay cards use plain `<p>` tags; this overlay uses `<h3>` + `<p>` inside `<div>` wrappers since it has titled sections):

```html
<div>
  <h3 class="font-bold text-lg mb-2">{{ "file-proto-h-1" | i18n({}, lang) | safe }}</h3>
  <p>{{ "file-proto-p-1" | i18n({}, lang) | safe }}</p>
</div>
<div>
  <h3 class="font-bold text-lg mb-2">{{ "file-proto-h-2" | i18n({}, lang) | safe }}</h3>
  <p>{{ "file-proto-p-2" | i18n({}, lang) | safe }}</p>
</div>
<div>
  <h3 class="font-bold text-lg mb-2">{{ "file-proto-h-3" | i18n({}, lang) | safe }}</h3>
  <p>{{ "file-proto-p-3" | i18n({}, lang) | safe }}</p>
</div>
<div>
  <h3 class="font-bold text-lg mb-2">{{ "file-proto-h-4" | i18n({}, lang) | safe }}</h3>
  <p>{{ "file-proto-p-4" | i18n({}, lang) | safe }}</p>
</div>
<div>
  <h3 class="font-bold text-lg mb-2">{{ "file-proto-h-5" | i18n({}, lang) | safe }}</h3>
  <p>{{ "file-proto-p-5" | i18n({}, lang) | safe }}</p>
</div>
<p class="mt-4">
  <a href="https://github.com/simplex-chat/simplexmq/blob/stable/protocol/xftp.md" target="_blank" rel="noopener" class="text-active-blue hover:underline">
    {{ "file-proto-spec" | i18n({}, lang) | safe }}
  </a>
</p>
```

### Step 5: Add navbar link

**Modify**: `website/src/_includes/navbar.html`

After Directory `<li>` block (after line 27, before the `<hr>` at line 29):
```html
<hr>
<li class="nav-link {% if active_file %}active{% endif %}">
    <a href="/file/">
        <span class="nav-link-text">{{ "file" | i18n({}, lang) | safe }}</span>
    </a>
</li>
```

Add `and ('file' not in page.url)` to language-selector exclusion condition (line 137):
```
{% if ('blog' not in page.url) and ('about' not in page.url) and ('donate' not in page.url) and ('privacy' not in page.url) and ('directory' not in page.url) and ('vouchers' not in page.url) and ('file' not in page.url) %}
```

### Step 6: Add translation keys

**Modify**: `website/langs/en.json` — add these keys:

```
Navbar:
  "file": "File"

Noscript + static page content:
  "file-noscript": "JavaScript is required for file transfer."
  "file-e2e-note": "End-to-end encrypted — the server never sees your file."
  "file-learn-more": "Learn more about XFTP protocol"
  "file-cta-heading": "Get SimpleX — the most private messenger"
  "file-cta-subheading": "The file transfer you just used is built on the same protocol as SimpleX Chat — end-to-end encrypted messaging, voice and video calls, groups, and file sharing. No user IDs. No phone numbers."

i18n bridge (fed to bundle via window.__XFTP_I18N__):
  "file-title": "SimpleX File Transfer"
  "file-drop-text": "Drag & drop a file here"
  "file-drop-hint": "or"
  "file-choose": "Choose file"
  "file-max-size": "Max 100 MB — the <a href=\"#join-simplex\">SimpleX app</a> supports up to 1 GB"
  "file-encrypting": "Encrypting\u2026"
  "file-uploading": "Uploading\u2026"
  "file-cancel": "Cancel"
  "file-uploaded": "File uploaded"
  "file-copy": "Copy"
  "file-copied": "Copied!"
  "file-share": "Share"
  "file-expiry": "Files are typically available for 48 hours."
  "file-sec-1": "Your file was encrypted in the browser before upload — the server never sees file contents."
  "file-sec-2": "The link contains the decryption key in the hash fragment, which the browser never sends to any server."
  "file-sec-3": "For maximum security, use the <a href=\"https://simplex.chat\" target=\"_blank\" rel=\"noopener\">SimpleX app</a>."
  "file-retry": "Retry"
  "file-downloading": "Downloading\u2026"
  "file-decrypting": "Decrypting\u2026"
  "file-download-complete": "Download complete"
  "file-download-btn": "Download"
  "file-too-large": "File too large (%size%). Maximum is 100 MB. The <a href=\"#join-simplex\">SimpleX app</a> supports files up to 1 GB."
  "file-empty": "File is empty."
  "file-invalid-link": "Invalid or corrupted link."
  "file-init-error": "Failed to initialize: %error%"
  "file-available": "File available (~%size%)"
  "file-dl-sec-1": "This file is encrypted \u2014 the server never sees file contents."
  "file-dl-sec-2": "The decryption key is in the link\u2019s hash fragment, which your browser never sends to any server."
  "file-dl-sec-3": "For maximum security, use the <a href=\"https://simplex.chat\" target=\"_blank\" rel=\"noopener\">SimpleX app</a>."
  "file-workers-required": "Web Workers required \u2014 update your browser"

Protocol overlay content:
  "file-protocol-title": "Why XFTP is the most private file transfer"
  "file-proto-h-1": "No accounts, no identifiers"
  "file-proto-p-1": "Each file chunk uses a fresh, random credential that is used once and discarded. The server has no concept of \"users\" — it only sees isolated, anonymous chunk operations."
  "file-proto-h-2": "Encrypted in your browser"
  "file-proto-p-2": "The entire file is encrypted with a random key before upload. The server stores ciphertext it cannot decrypt. The key travels only in the URL fragment, which browsers never send to any server."
  "file-proto-h-3": "Triple encryption"
  "file-proto-p-3": "Every transfer has three layers: TLS transport encryption, per-recipient transit encryption (unique ephemeral key exchange per download), and file-level end-to-end encryption."
  "file-proto-h-4": "Distributed across independent servers"
  "file-proto-p-4": "File chunks are split across servers operated by independent parties. No single operator sees all chunks. Even if one operator is compromised, they only see encrypted fragments."
  "file-proto-h-5": "Files expire automatically"
  "file-proto-p-5": "Files are deleted after approximately 48 hours. There is no persistent storage, no file management, no way to extend expiration. Ephemeral by design."
  "file-proto-spec": "Read the XFTP protocol specification →"
```

### Step 7: Update .eleventy.js

**Modify**: `website/.eleventy.js`

1. Add `"file"` to `supportedRoutes` array (line 56):
```js
const supportedRoutes = ["blog", "contact", "invitation", "messaging", "docs", "fdroid", "file", ""]
```

2. Add passthrough copy (after line 306, with the other `addPassthroughCopy` calls):
```js
ty.addPassthroughCopy("src/file-assets")
```

### Step 8: Gitignore

**Modify**: `.gitignore` (project root) — add:
```
website/src/file-assets/
```

### Step 9: Fix script.js null guard

**Modify**: `website/src/js/script.js`

The `openOverlay()` function (line 180) crashes when the URL hash is an XFTP URI fragment (e.g. `#simplex:...`) because `document.getElementById('simplex:...')` returns null, and `el.classList.contains('overlay')` throws a TypeError on null.

**Change** (line 184-185):
```js
// Before:
const el = document.getElementById(id)
if (el.classList.contains('overlay')) {

// After:
const el = document.getElementById(id)
if (el && el.classList.contains('overlay')) {
```

This is a one-character change (`if (el.classList` → `if (el && el.classList`). It makes `openOverlay()` safely ignore hash fragments that don't correspond to overlay elements — which is correct behavior regardless of the file page (any non-overlay hash should be silently ignored).

---

## 5. Known Divergences from Product Plan

These are intentional deviations from the product plan, caused by browser constraints or library limitations:

1. **Download requires a click**: Product plan says "No intermediate 'click to download' step." The bundle shows a "Download" button instead of auto-starting. This is a browser security constraint — triggering a file download requires a user gesture. The button also lets the user see file metadata before downloading.

2. **No cancel during download**: Product plan specifies a Cancel button during download. The bundle does not implement this. The download is relatively fast (direct HTTPS) and cancellation can be done by closing the tab.

3. **Protocol diagram deferred**: Product plan describes a protocol flow diagram in the overlay. SVG diagrams are deferred to a future iteration. The overlay ships with text-only content (`showImage: false`).

4. **Overlay close clears download hash**: When the protocol overlay is opened and closed during download mode, `closeOverlay()` clears the URL hash. This is cosmetic — the bundle already parsed the hash on init and the download is unaffected. The URL bar loses the fragment, but the user received the link from elsewhere and doesn't need to re-copy it.

---

## 6. Verification

### Build
```bash
cd website
npm install --ignore-scripts
mkdir -p src/file-assets
cp node_modules/@shhhum/xftp-web/dist-web/assets/{index.js,index.css,crypto.worker.js} src/file-assets/
npm run build
ls _site/file/index.html _site/file-assets/index.js _site/file-assets/index.css _site/file-assets/crypto.worker.js
```

### Manual test checklist
```
Visit /file/
 1. Navbar "File" link is active
 2. <noscript> message hidden (JS enabled)
 3. Drop zone visible, dark mode works
 4. "End-to-end encrypted" note visible below drop zone
 5. maxSizeHint shows "Max 100 MB — the SimpleX app supports up to 1 GB" with link
 6. Upload small file → progress ring → share link + Copy/Share buttons
 7. Inline CTA appears after upload with 5 app store buttons
 8. Copy link → new tab → download page → Download button → file saved
 9. Inline CTA appears after download
10. "Learn more" → overlay opens (no hash change, no console error)
11. Overlay shows 5 protocol blocks + spec link
12. Close overlay → file tool unaffected, no JS error
13. Bottom CTA shows 5 app store buttons
14. Visit /file/#invalid → no JS crash (null guard works)
15. Language selector hidden on /file/ page
```

### No regressions
```bash
# Verify other pages still build
ls _site/index.html _site/directory/index.html _site/blog/index.html
# Verify overlay still works on homepage (openOverlay null guard is backward-compatible)
```
