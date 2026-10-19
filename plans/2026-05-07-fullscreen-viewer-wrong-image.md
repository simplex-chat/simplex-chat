# Fullscreen image viewer: opens the wrong image

Design doc for the fix shipped in PR #6869.

## Problem

The fullscreen image viewer occasionally opened the chat's oldest media
instead of the image the user tapped. Reproductions were intermittent —
the gating condition turned out to be the runtime state of the
*immediately-older* sibling of the tapped item.

## Background — pager state model

`providerForGallery` (`apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt:3537`)
backs the fullscreen viewer with a virtual pager of 10000 pages. The
pager's state is two variables, captured in a closure:

- `initialIndex` — pager page that maps to the anchor item; starts at 5000.
- `initialChatId` — id of the anchored chat item; starts at the tapped item.

Invariant: page `initialIndex` always shows item `initialChatId`. Other
pages are computed by walking `chatItems` older / newer from the anchor
via the local `item()` helper.

`scrollToStart()` is called by `ImageFullScreenView.kt` to lock the
pager's leftward boundary at the user's current item, in two situations:

- **Init probe** (`ImageFullScreenView.kt:48-55`) — at viewer open, if
  `getMedia(initialIndex - 1) == null` (no older sibling reachable),
  reposition so the tapped item becomes page 0.
- **Runtime branch** (`ImageFullScreenView.kt:97-112`) — during scroll,
  if `getMedia(index) == null` while the user is at `index + 1`, lock
  the pager so the null page isn't reachable.

Both callers want the same outcome: **page 0 = the user's current
anchor item**, leftward = unreachable.

## Root cause

Pre-fix body of `scrollToStart`:

```kotlin
override fun scrollToStart() {
  initialIndex = 0
  initialChatId = chatItems.firstOrNull { canShowMedia(it) }?.id ?: return
}
```

The second line rewrote `initialChatId` to the chat's *oldest showable
media* — not the user's current anchor. This mismatched what both
callers wanted. It happened to coincide with the correct behavior when
the anchor already was the chat's oldest showable, which is why the bug
masked itself for years.

The bug surfaced when the init probe fired for a non-boundary reason:

- The immediately-older sibling existed and passed `canShowMedia` (file
  marked loaded; file path resolved or remote was connected).
- But `getLoadedImage` returned `null` at decode time (undecodable
  bytes, missing file on disk, crypto error).
- `getMedia(initialIndex - 1)` therefore returned `null`.
- The probe misread that null as "no older sibling exists" and called
  `scrollToStart()`.
- `scrollToStart` rewrote `initialChatId` to the chat's oldest showable.
- Page 0 of the pager rendered that oldest item — the wrong image.

## Fix

Delete the second line. `scrollToStart` becomes:

```kotlin
override fun scrollToStart() {
  initialIndex = 0
}
```

`initialChatId` is preserved across the call. Page 0 now maps to the
current anchor — exactly what both callers wanted from the start.

## Why this is correct for both callers

- **Init probe.** Before the call, `initialChatId` is the tapped item.
  After the call, page 0 = tapped item. ✓
- **Runtime branch.** Before the call, `currentPageChanged` has already
  updated `initialChatId` to the user's currently visible item. After
  the call, page 0 = current item; the user's view is preserved with no
  visible jump. (Pre-fix the user got teleported to the chat's oldest
  media when a null sibling tripped this branch — a latent UX bug
  resolved by the same one-line change.)

## Why a wider structural change is not in scope here

`getMedia` returns `null` for two distinct conditions: (a) navigation
found no showable item, (b) navigation found one but decode failed. A
deeper refactor would let consumers distinguish these. That refactor is
deliberately out of scope for this fix:

- The user-visible bug (wrong image) is fully resolved by the one-line
  change. No additional code is required to address the report.
- The remaining symptom — locking the user out of older loadable items
  behind one that fails to decode — is mild, pre-existing, and not part
  of the report. If it becomes user-visible, address it in a follow-up.
- A wider refactor would expand the diff, the review surface, and the
  regression risk for a fix that needs to ship promptly.
- `good-code-v5.md`: *"Find the minimal change. The smallest structural
  modification that achieves the goal."* The smallest modification that
  resolves the reported bug is the deletion of one line.

## Verification

`apps/multiplatform/common/src/commonTest/kotlin/chat/simplex/app/ProviderForGalleryTest.kt`:

- `testScrollToStartPreservesAnchor` — drives the public provider
  interface: moves the anchor off `cItemId` via `currentPageChanged`,
  calls `scrollToStart`, then reads the anchor back through `onDismiss`'s
  `scrollTo` callback. Pre-fix would observe `scrollTo(2)` (the chat's
  oldest); post-fix `scrollTo(1)` (anchor preserved).
- `testOnDismissOnActiveItemDoesNotScroll` — pins the `onDismiss`
  early-return contract that the regression test reads through.

Manual sanity (Android + desktop): tap newest / oldest / a middle image
in a chat with multiple media — fullscreen opens on the tapped image in
each case; swipe in both directions still works.

## Alternatives considered and rejected

- **Distinguish "no item" from "load failed" inside `getMedia`.**
  Requires either a return-type redesign (sealed result type) or an
  added query method on the interface. Both expand the diff well beyond
  what the user-visible bug requires. Deferred to a possible follow-up
  if the milder remaining symptom is reported.
- **Hoist the local `item()` helper to a top-level testable function.**
  The regression test exercises the public provider interface and
  reads the anchor back via `onDismiss`'s `scrollTo` callback, so no
  internal extraction is needed for testability.
