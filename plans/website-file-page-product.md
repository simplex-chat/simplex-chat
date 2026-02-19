# SimpleX File Transfer — Product Plan

## 1. Why this exists

There is no way to privately share a file with someone without both of you installing software.

Every existing option fails at this:

- **WeTransfer, Google Drive, Dropbox** — require accounts, the service sees your files, tracks everything
- **Signal, WhatsApp** — both parties need the app and a phone number
- **OnionShare** — requires Tor, technical setup, sender must stay online
- **Firefox Send** — was the closest thing, and Mozilla killed it

The gap is obvious: a person should be able to share a file privately by opening a webpage and dropping a file. The recipient should be able to download it by clicking a link. Neither should need an account, an app, or trust that the server isn't reading their files.

SimpleX already solves the hard problems — XFTP protocol, client-side encryption, decentralized servers with no user identifiers. This product is just putting a browser UI in front of that infrastructure.

The key technical insight that makes this work: the decryption key lives in the URL hash fragment (`#...`). Browsers never send the hash to the server. So the link itself is the access credential, and the server that stores the file cannot decrypt it.

## 2. Users

**Primary: People who need to share something privately, right now.**

They don't want to create an account. They don't want to install an app. They don't want to think about encryption. They just need to get a file from point A to point B without anyone else seeing it.

Concrete scenarios:

- A source sends a document to a journalist. They can't install SimpleX without raising suspicion. They open a webpage, drop the file, send the link over whatever channel they have.
- A person shares medical records with a family member. They don't want Google or Dropbox to have their health data.
- Someone sends a contract to a lawyer. The contents are privileged. Using a service that can read the file violates that privilege.
- A SimpleX user wants to share a photo with a friend who doesn't have the app yet. Instead of "download SimpleX first", they send a link.
- Someone at work needs to share a file that's too sensitive for Slack or email. They don't have time to set up a secure channel.

**What these users have in common**: They value privacy but won't tolerate friction. If it takes more than 10 seconds to understand what to do, they'll use Google Drive instead. The product must be as easy as the insecure alternatives.

**Secondary: Privacy advocates and technical users** who will share this tool with others precisely because of its security properties. These users care about the hash fragment trick, the XFTP protocol, the fact that servers are operated by multiple independent parties. They are the distribution channel — they recommend this to the primary users above.

## 3. Problems solved

### The account wall

Every file sharing service wants you to create an account before you can share anything. Accounts mean identity. Identity means tracking. The whole point of private file sharing is defeated before you upload a single byte.

This product has no accounts. You open the page. You drop a file. You get a link. Done.

### The server trust problem

When you upload a file to Google Drive or WeTransfer, you're trusting that company not to read your file, not to hand it to governments, not to get breached. That trust is routinely violated.

Here, the file is encrypted in the browser before it leaves. The server stores ciphertext. Even if the server is compromised, seized, or malicious, the attacker gets encrypted noise. The decryption key only exists in the URL that the sender shares directly with the recipient.

### The metadata problem

Most "encrypted" services still collect metadata — who uploaded, when, from what IP, who downloaded. Metadata is often more revealing than content.

XFTP servers have no user accounts, no persistent identifiers. Files are stored as encrypted chunks distributed across multiple servers operated by independent parties (SimpleX and Flux). No single operator sees the complete picture.

### The installation barrier

Secure tools usually require installation. Installation requires trust, time, and technical ability. For many real-world scenarios (sources contacting journalists, one-time file shares between strangers), the installation barrier kills adoption.

This works in a browser tab. The recipient clicks a link, the file downloads. They don't need to know what XFTP is, what SimpleX is, or how encryption works.

### The permanence problem

Files shared through most services persist indefinitely unless manually deleted. This creates long-term risk — a breach years later exposes files shared years ago.

XFTP files expire after approximately 48 hours. The data is ephemeral by design. Share it, download it, it's gone.

## 4. UX

### Design principles

1. **Zero decisions.** The user should never have to choose between options, configure settings, or understand the system. Upload gives you a link. Link gives you a file. That's it.

2. **Transparency without preaching.** Show the user what's happening (encrypting, uploading, done) and briefly explain the security properties. Don't lecture. Don't use jargon. Don't make them feel like they're using a "security tool" — it should feel like a normal file sharing tool that happens to be private.

3. **Every element earns its place.** No decorative illustrations, no marketing copy, no "features" section. The page is a tool. It has a drop zone, a progress indicator, and a share link. Nothing else.

4. **Match the website.** Same navbar, same dark mode, same Tailwind classes, same typography. The file page should feel like a natural part of simplex.chat, not a bolted-on app.

### Upload flow — step by step

**State 1: Ready (drop zone)**

The page loads with a centered card containing:

- A drop zone with a dashed border — "Drag & drop a file here"
- The word "or" in muted text
- A "Choose file" button (for mobile and accessibility)
- "Max 100 MB" as a hint below

Below the drop zone, a single line of muted text: "End-to-end encrypted — the server never sees your file." This tells privacy-conscious users what they need to know before they commit to uploading. No click-through, no "learn more" — just one sentence.

The drop zone is the entire focus of the page. There's nothing else competing for attention.

When the user drags a file over the drop zone, the border highlights (color change + subtle animation). This confirms "yes, you can drop here."

**State 2: Encrypting + uploading (progress)**

The moment a file is selected, the drop zone disappears and is replaced by:

- A circular progress ring (canvas-drawn, smooth animation)
- Status text below: "Encrypting..." then "Uploading..."
- A "Cancel" button

The progress ring fills continuously:
- 0–30%: encryption progress (from `encryptFileForUpload`'s `onProgress` callback)
- 30–100%: upload progress (from `uploadFile`'s `onProgress` callback)

The split is weighted because encryption is CPU-bound and fast (a few seconds for 100 MB), while upload depends on network speed and is usually the longer phase.

The cancel button aborts the operation and returns to the drop zone. No confirmation dialog — canceling should be instant and low-stakes.

**State 3: Complete (share link)**

After successful upload:

- "File uploaded" in success styling
- A text input containing the full share URL, pre-selected for easy copying
- A "Copy" button next to it — clicking changes text to "Copied!" for 2 seconds
- On mobile: a "Share" button using the Web Share API (`navigator.share()`) which opens the native share sheet (WhatsApp, Telegram, AirDrop, etc.) — much better than copy-pasting a long URL on a phone. Falls back to Copy if Web Share is unavailable.
- "Files are typically available for 48 hours." as a subtle hint
- A security note (three short lines):
  - "Your file was encrypted in the browser before upload — the server never sees file contents."
  - "The link contains the decryption key in the hash fragment, which the browser never sends to any server."
  - "For maximum security, use the [SimpleX app](https://simplex.chat)."

The share link is the hero of this state. The URL input should be visually prominent — large, full-width, easy to select on mobile. The Copy button should be right next to it, obvious, and satisfying to click.

**State 4: Error**

If something goes wrong:

- An error message describing what happened (in plain language, not technical codes)
- A "Retry" button — IF the error is retriable (network timeout, connection reset)
- No retry button for permanent errors (file too large, empty file, server rejected)

The retry button re-attempts the upload with the same file. The user doesn't need to re-select it.

### Download flow — step by step

**State 1: Downloading (progress)**

When the page loads with a hash fragment (`/file/#...`), it immediately starts downloading:

- A circular progress ring (same visual as upload)
- Status text: "Downloading..."
- A "Cancel" button

No intermediate "click to download" step. The user clicked the link — they want the file. Start immediately.

**State 2: Complete (save)**

After successful download:

- Trigger the browser's native save dialog with the original filename
- Show "File downloaded" confirmation
- The same security note as the upload page

The file content is decrypted entirely in the browser. The plaintext never touches a server.

**State 3: Error**

- "This file is no longer available" — for expired/deleted files (permanent, no retry)
- "Download failed" with retry — for network errors (retriable)

### Edge cases

**File too large (>100 MB):** Error shown immediately after file selection, before any encryption or upload begins. "File too large (X MB). Maximum is 100 MB."

**Empty file (0 bytes):** Rejected immediately. "File is empty."

**JavaScript disabled:** The `<noscript>` tag inside `#file-app` shows "JavaScript is required."

**Slow connection:** The progress ring provides continuous feedback. The cancel button is always available. No timeouts that silently kill the transfer.

**Browser back/forward:** Hash-based routing means the browser's back button works naturally. Going back from a share link returns to upload mode.

**Mobile:** The "Choose file" button works on mobile (triggers the native file picker). Drag & drop is desktop-only but the button provides equivalent access. The layout is responsive — the card fills available width on small screens.

**Dark mode:** Follows the website's existing dark mode toggle (`.dark` class on `<html>`). The progress ring colors adapt. All text and backgrounds use the website's existing Tailwind color palette.

**Malformed link:** If someone visits `/file/#garbage` (truncated URL, corrupted link, random text), `decodeDescriptionURI` will throw. Show: "Invalid link — the file link appears to be incomplete or corrupted." No retry button (there's nothing to retry). This is distinct from "file not found" (valid link, expired file) — wording matters because it tells the user whether to ask the sender for a new link vs. wait and retry.

**Link truncation:** The share URI can be long (hundreds of characters of base64url). Some channels truncate long URLs — SMS, Twitter/X, some email clients. The library's redirect compression (`uploadFile` automatically uses redirect descriptions when the URI exceeds ~400 characters) mitigates this for multi-chunk files. For single-chunk files (small files), the URI is naturally short. There's no user-facing action needed, but the security note could mention "copy the complete link" as a hint.

**Internationalization:** The website supports multiple languages, but all UI strings in the file page (status messages, error messages, button labels) are hardcoded in English in the JS source. This is acceptable for v1 — the file page is language-excluded in the navbar (no language selector shown), matching the directory page pattern. i18n support can be added later by externalizing strings.

### Abuse and moderation

The server stores encrypted bytes and cannot inspect file contents. This is the core privacy guarantee. But it also means the server cannot scan for illegal content (CSAM, malware, etc.).

Mitigations:

- **48-hour expiration** — files are automatically deleted. The system is not suitable for persistent hosting of any content.
- **Rate limiting** — XFTP servers enforce per-IP upload rate limits and storage quotas, preventing bulk abuse.
- **No directory or discovery** — there's no public listing, no search, no way to discover files. You need the exact link. This isn't a distribution platform.
- **Abuse reporting** — a reporting mechanism can be added later (e.g., "Report abuse" link that accepts a file URI). The server operator can delete specific chunks by their ID without decrypting contents.
- **Legal compliance** — server operators (SimpleX and Flux) handle takedown requests under their respective jurisdictions. Since they can delete chunks by ID, they can comply with valid legal orders without compromising the E2E encryption of other files.

This is the same trade-off every E2E encrypted service makes (Signal, WhatsApp, iMessage). The privacy guarantee is worth the moderation limitation. The ephemeral nature (48h expiry) significantly reduces the risk compared to permanent storage.

### What this is NOT

- **Not a file storage service.** Files expire. There's no file list, no dashboard, no management.
- **Not a collaboration tool.** One sender, one link, one or more recipients. No comments, no versioning.
- **Not a replacement for SimpleX Chat.** The security note points users to the app. This is the on-ramp, not the destination.
