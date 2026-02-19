# SimpleX File Transfer — Product Plan

## Table of Contents

1. [Strategic purpose](#1-strategic-purpose)
2. [Users and conversion funnel](#2-users-and-conversion-funnel)
3. [Why XFTP is the most private file transfer protocol](#3-why-xftp-is-the-most-private-file-transfer-protocol)
4. [Page structure and UX](#4-page-structure-and-ux)
5. [Upload flow](#5-upload-flow)
6. [Download flow](#6-download-flow)
7. [Edge cases](#7-edge-cases)
8. [Abuse and moderation](#8-abuse-and-moderation)
9. [What this is NOT](#9-what-this-is-not)

---

## 1. Strategic purpose

This page is a **conversion point**. Its primary goal is to get people to download the SimpleX app.

The file transfer is a **live demo** — proof that SimpleX's encryption and privacy infrastructure actually works, right in the browser, without installing anything. The user experiences the technology firsthand: they drop a file, it gets encrypted in their browser, uploaded to XFTP servers with no accounts or identifiers, and the recipient downloads it with a link that contains the decryption key in the URL fragment (which browsers never send to any server).

After experiencing this, the page makes the case: *what you just used is a tiny fraction of what the SimpleX app does. The app uses the same protocol — and much more — for messaging, calls, groups, and file sharing. No user IDs. No phone numbers. The most private communication platform that exists.*

**The page serves three functions, in priority order:**

1. **Demonstrate** — let people experience XFTP encryption firsthand, proving it works
2. **Educate** — explain why the XFTP protocol is the most private file transfer protocol in existence
3. **Convert** — drive app downloads with a clear, compelling call to action

The file transfer tool by itself is useful — but it exists to showcase the protocol and funnel users to the app.

---

## 2. Users and conversion funnel

### Who arrives at this page

**Path 1: Referred by a SimpleX user.** Someone sends them a file link. They click it, download a file, and land on a page that explains what just happened and why they should get the app. This is the highest-intent path — they've already interacted with the SimpleX ecosystem.

**Path 2: Privacy-curious visitors.** People browsing simplex.chat who click "File" in the navbar. They're already interested in SimpleX, and the file transfer demo gives them something to try immediately, without commitment.

**Path 3: Linked from external sources.** Privacy advocates, journalists, security researchers who share the page as "the most private way to send a file without installing anything." These users become evangelists.

### The conversion funnel

```
See the page → Try the demo → Read why it's secure → Download the app
```

Each step must flow naturally into the next. The demo creates curiosity ("how does this work?"), the protocol explanation builds trust ("this is genuinely private"), and the CTA captures intent ("I want this for all my communication").

### Concrete scenarios

- A journalist receives a file link from a source. After downloading, they see the security explanation and realize SimpleX is what they need for source communication. They download the app.
- A privacy-conscious person uploads a file to share medical records. While waiting for the upload, they read the protocol section. They're impressed and download the app.
- A tech blogger finds the page, tries it, and writes about it — linking to the page and driving more traffic into the funnel.
- A SimpleX user sends a file to a friend who doesn't have the app. The friend downloads the file, reads "this is what SimpleX does — and the app does much more," and installs it.

---

## 3. Why XFTP is the most private file transfer protocol

This section defines the protocol properties that the page must communicate to users. These are the facts that make the case for downloading the app.

### No user identifiers at any level

XFTP has no accounts, no usernames, no email addresses, no phone numbers, no device tokens, no persistent IDs of any kind. Not even random numbers. Each file chunk uses a fresh, random credential that is used once and discarded. The server has no concept of "users" — it only sees isolated, anonymous chunk operations.

This is not a feature bolted onto an existing system. The protocol was designed from the ground up to have no identifiers. SimpleX is the first communication platform to achieve this.

### Triple encryption

Every file transfer has three independent layers of encryption:

1. **TLS transport** — standard HTTPS encryption for the network connection
2. **Per-recipient transit encryption** — each download uses a unique ephemeral key exchange, so the ciphertext is different for every recipient, even for the same chunk. There are no identifiers or ciphertext in common between sent and received traffic — even if TLS is compromised, traffic correlation is frustrated.
3. **File-level end-to-end encryption** — the entire file is encrypted with a random key before upload. The key is in the URL fragment, which browsers never send to any server.

No other file transfer service has all three layers. Most have only TLS.

### Traffic correlation resistance

The protocol is designed so that even if TLS is compromised, an attacker monitoring server traffic cannot easily correlate senders and recipients:

- **No shared ciphertext**: each recipient's download is encrypted with a unique key. The bytes going in (upload) and coming out (download) are completely different, even for the same chunk.
- **No shared identifiers**: sender and recipient IDs are different random values for every chunk.
- **Fixed chunk sizes**: files are split into fixed-size chunks. A large file looks indistinguishable from many small files to the server.

### Multi-operator, zero-trust architecture

File chunks are distributed across servers operated by independent parties (SimpleX and Flux). No single operator sees all chunks of a file. Even if one operator is compromised, they only see encrypted fragments with no way to reconstruct the file.

Users can also self-host XFTP servers — the protocol is open and the server software is open-source.

### Deniability

Recipients cannot cryptographically prove to a third party that a file came from a specific sender. Two contacts cannot collaborate to confirm they are communicating with the same person, even if they receive the same file. This is a protocol-level guarantee, not a policy.

### Protocol-mandated privacy

The XFTP protocol specification *requires* that servers:
- Do NOT log client commands or transport connections
- Do NOT store history of retrieved files
- Do NOT create database snapshots
- Do NOT store any information that may compromise privacy or forward secrecy

This is not a "we promise not to log" policy — it's a protocol requirement that any compliant server implementation must follow.

### Ephemeral by design

Files expire automatically (approximately 48 hours). There is no persistent storage, no file management, no way to extend expiration. The system is designed for transient transfer, not storage. A breach years later finds nothing.

### How this compares

| Property | XFTP (SimpleX) | WeTransfer | Google Drive | Signal | OnionShare |
|----------|----------------|------------|-------------|--------|------------|
| No accounts required | Yes | No | No | No (phone number) | Yes |
| No user identifiers | Yes | No | No | No | Partial |
| E2E encryption | Yes (3 layers) | No | No | Yes (1 layer) | Yes (1 layer) |
| Server cannot read files | Yes | No | No | Yes | Yes |
| Traffic correlation resistance | Yes | No | No | No | Partial (Tor) |
| Multi-operator distribution | Yes | No | No | No | No |
| No installation required (sender) | Yes | No | No | No | No |
| No installation required (recipient) | Yes | Yes | Yes | No | No |
| Deniability | Yes | No | No | Yes | No |
| Open protocol specification | Yes | No | No | Yes | Yes |
| Auto-expiring files | Yes | Partial | No | N/A | Yes |

---

## 4. Page structure and UX

### Design principles

1. **Demo first, educate second, convert third.** The file transfer tool is at the top. The protocol explanation is below. The app download CTA is at the bottom (and repeated after upload/download completes). The user experiences before they learn, and learns before they're asked to act.

2. **Zero decisions.** The upload/download tool has no settings, no options, no configuration. Drop a file, get a link. Click a link, get a file.

3. **Transparency without jargon.** Show what's happening (encrypting, uploading, done) and explain the security properties in plain language. Technical users can read the protocol spec linked at the bottom.

4. **Match the website.** Same navbar, dark mode, typography. The file page is a natural part of simplex.chat.

### Page layout (top to bottom)

**1. Title**: "SimpleX File Transfer"

**2. File transfer tool**: The upload drop zone or download progress — this is the interactive demo. After a successful upload or download, an inline app download CTA appears directly below the completion state.

**3. Protocol section: "Why this is the most private file transfer"**

After the tool, a section explaining the XFTP protocol security properties. Not a wall of text — 4-5 short, scannable blocks with bold titles:

- **No accounts, no identifiers** — one sentence explaining anonymous per-chunk credentials
- **Encrypted in your browser** — the server stores ciphertext it cannot decrypt; the key is in the link
- **Triple encryption** — TLS + per-recipient transit encryption + file-level E2E
- **Distributed across independent servers** — chunks split across SimpleX and Flux operators; no single operator sees the complete file
- **Files expire automatically** — approximately 48 hours, ephemeral by design

Each block is 2-3 sentences. The whole section fits on one screen. No expand/collapse, no "learn more" — just the facts.

A subtle link at the end: "Read the XFTP protocol specification for the full technical details."

**4. App download CTA**

The conversion section. Clear, prominent, centered:

- Heading: "Get SimpleX — the most private messenger"
- Subheading: "The file transfer you just used is built on the same protocol as SimpleX Chat — end-to-end encrypted messaging, voice and video calls, groups, and file sharing. No user IDs. No phone numbers."
- App store buttons (Apple Store, Google Play, F-Droid, TestFlight, APK)

This section appears at the bottom of the page AND is shown inline after a successful upload or download.

---

## 5. Upload flow

### State 1: Ready (drop zone)

A centered card with:

- A drop zone with a dashed border — "Drag & drop a file here"
- The word "or" in muted text
- A "Choose file" button (for mobile and accessibility)
- "Max 100 MB" as a hint below

Below the drop zone: "End-to-end encrypted — the server never sees your file."

When the user drags a file over, the border highlights.

### State 2: Encrypting + uploading (progress)

The drop zone is replaced by:

- A circular progress indicator
- Status text: "Encrypting..." then "Uploading..."
- A "Cancel" button

Cancel returns to the drop zone. No confirmation dialog.

### State 3: Complete (share link + CTA)

- "File uploaded" in success styling
- A text input containing the full share URL, pre-selected for easy copying
- A "Copy" button — clicking changes text to "Copied!" for 2 seconds
- On mobile: a "Share" button that opens the native share sheet (WhatsApp, Telegram, AirDrop, etc.). Falls back to Copy if unavailable.
- "Files are typically available for 48 hours."

Security note (three short lines):
- "Your file was encrypted in the browser before upload — the server never sees file contents."
- "The link contains the decryption key — your browser never sends it to any server."
- "For maximum security, use the SimpleX app."

**Inline CTA** below the security note — same prominence as the bottom-of-page CTA:
- Heading: "Get SimpleX — the most private messenger"
- Subheading about the app using the same protocol
- App store buttons

### State 4: Error

- An error message in plain language
- A "Retry" button for retriable errors (network timeout, connection reset)
- No retry button for permanent errors (file too large, empty file, server rejected)
- Retry re-attempts with the same file — no re-selection needed

---

## 6. Download flow

### State 1: Downloading (progress)

When the page loads with a file link, it immediately starts downloading:

- A circular progress indicator (same visual as upload)
- Status text: "Downloading..."
- A "Cancel" button

No intermediate "click to download" step. The user clicked the link — start immediately.

### State 2: Complete (save + CTA)

- The browser's native save dialog with the original filename
- "File downloaded" confirmation
- The same security note as the upload page

**Inline CTA** — same as upload completion. Same heading, subheading, and app store buttons. This is the highest-conversion moment — the user just experienced the technology working and is most receptive.

### State 3: Error

- "This file is no longer available" — for expired/deleted files (permanent, no retry)
- "Download failed" with retry — for network errors (retriable)
- "Invalid link — the file link appears to be incomplete or corrupted." — for malformed links (permanent, no retry)

---

## 7. Edge cases

**File too large (>100 MB):** Error shown immediately after file selection. "File too large (X MB). Maximum is 100 MB."

**Empty file (0 bytes):** Rejected immediately. "File is empty."

**JavaScript disabled:** A message saying "JavaScript is required."

**Slow connection:** Progress indicator provides continuous feedback. Cancel is always available. No silent timeouts.

**Browser back/forward:** Back button works naturally — returns to upload mode.

**Mobile:** "Choose file" button triggers native file picker. Drag & drop is desktop-only. Layout is responsive.

**Dark mode:** Follows the website's existing dark mode toggle.

**Malformed link:** "Invalid link — the file link appears to be incomplete or corrupted." No retry button. Distinct from "file not found" (expired) so the user knows whether to ask for a new link.

**Link truncation:** Long URLs can be truncated by SMS, Twitter/X, some email clients. The protocol's redirect compression mitigates this for larger files. Small files have naturally short URLs.

**Internationalization:** All UI strings are in English for v1. The page is excluded from the language selector, matching other single-language pages on the website. i18n can be added later.

---

## 8. Abuse and moderation

The server stores encrypted bytes and cannot inspect file contents. This is the core privacy guarantee.

Mitigations:

- **48-hour expiration** — files are automatically deleted. Not suitable for persistent hosting.
- **Rate limiting** — XFTP servers enforce per-IP upload rate limits and storage quotas.
- **No directory or discovery** — no public listing, no search. You need the exact link.
- **Abuse reporting** — can be added later. Server operators can delete specific file chunks without decrypting.
- **Legal compliance** — server operators handle takedown requests under their jurisdictions without compromising E2E encryption of other files.

Same trade-off as Signal, WhatsApp, iMessage. Ephemeral nature (48h) significantly reduces risk vs. permanent storage.

---

## 9. What this is NOT

- **Not a file storage service.** Files expire. No dashboard, no management.
- **Not a collaboration tool.** One sender, one link, one or more recipients.
- **Not the destination.** This is the on-ramp to SimpleX Chat. The demo proves the technology. The app is the product.
