# Fix: hand cursor not appearing over clickable commands (desktop)

## Problem

On desktop, hovering over a clickable command in a message (e.g. `/join 143556`) sometimes does not
change the mouse cursor to a hand. It usually works, but fails intermittently — most often when
clicking several commands in a row — and once wrong, the cursor stays wrong while moving within the
same message; it only recovers after leaving the message text and re-entering it.

## Investigation

The cursor is driven by two cooperating layers in `ClickableText` (TextItemView.kt):

1. **Detection** — `Modifier.pointerInput { detectCursorMove { ... } }` maps the pointer position to
   a character offset and checks for `COMMAND`/link annotations, storing `PointerIcon.Hand` or
   `PointerIcon.Text` in per-item state (`onHover`).
2. **Display** — `Modifier.pointerHoverIcon(icon.value)` tells Compose which cursor to show.

Both layers were traced against the Compose Multiplatform 1.8.2 sources (`ui-desktop`,
`foundation-desktop`).

### Defect 1: detection layer lost events (commit "fix pointer cursor not changing to hand…")

`detectCursorMove` (GestureDetector.kt) processed **one pointer event per
`awaitPointerEventScope` block**, exiting and re-entering the scope through `forEachGesture` for
every event. `forEachGesture` is deprecated in this exact Compose version with the message *"Use
awaitEachGesture instead. forEachGesture() can drop events between gestures."* — so the final Move
event, the one that should switch the cursor when the pointer comes to rest on a command, could be
silently dropped. It also ignored `Enter` events entirely, so when the chat list shifted under a
stationary cursor (every command click appends the sent message), the newly hovered item — which
receives `Enter`, not `Move` — never updated the icon state.

**Fix:** one never-exiting `awaitPointerEventScope` loop reacting to both `Move` and `Enter`.

### Defect 2: display layer silently swallows updates (commit "set hover cursor directly…")

This made hovering reliable but the cursor could still get stuck, because Compose's
`pointerHoverIcon` display side is edge-triggered with several silent-drop guards
(all confirmed in the CMP 1.8.2 sources):

- `HoverIconModifierNode` acts only on per-node `Enter`/`Exit` events (`PointerIcon.kt:253-259`);
  Move events never refresh the displayed cursor.
- Its `icon` setter displays only if the value *changed* **and** the node is marked in-bounds
  (`PointerIcon.kt:213-221`) — re-asserting the same value, or changing it while the node believes
  the cursor is outside, is silently dropped.
- `setIcon` writes the AWT cursor immediately, last-write-wins, with no later reconciliation
  (`RootNodeOwner.skiko.kt:738-744`); `onDetach`/`onCancelPointerInput` reset it to default from
  recomposition, outside pointer dispatch (`PointerIcon.kt:278-288`).
- Whether a node gets `Enter` depends on `hasEntered` state stored separately in
  `HitPathTracker.Node` (`HitPathTracker.kt:582-594`); confirmed windows exist where the two
  desync (detach between `buildCache` and dispatch; `cleanUpHover` clearing `isIn` without
  dispatching; Press/Release never remapping to Enter). Once desynced, the node receives only
  Moves — which it ignores — so the hand can never be displayed again until the pointer physically
  leaves and re-enters the text.
- Recovery after content shifts under a stationary cursor depends entirely on the synthetic Move
  (`SyntheticEventSender`), which is skipped whenever the sender was reset or
  `needUpdatePointerPosition` was not set for that frame.

Clicking many commands maximizes exposure: each click inserts a message → recomposition burst +
list shift + (for `/join`) group-state changes — each transition can hit one of these edges.
Related upstream issues: JetBrains/compose-multiplatform #2091, #1314, #3750.

**Fix:** in `onHover` — which now fires reliably on every Move/Enter over the text — set the AWT
cursor imperatively on desktop (`desktopSetHoverCursor`), in addition to the existing
`pointerHoverIcon` modifier (kept for the normal reset-to-default on exit). The write targets the
same Skia canvas component Compose writes to (`ComposeSceneMediator.setPointerIcon` →
`contentComponent.cursor`), so last-write-wins stays consistent with the framework's own updates.
No framework edge can leave the cursor stuck, because it is re-asserted on every mouse move.

## Performance

- `detectCursorMove` is now cheaper per event than before (no scope teardown/re-entry per event).
- `desktopSetHoverCursor` runs at mouse-move rate: steady-state cost is a cached component
  reference check (parent-chain walk, no allocations) plus an int comparison; the native
  `setCursor` call fires only when the cursor type actually changes. The canvas component lookup
  (recursive tree walk) runs once per window and is cached.
- No new recompositions: the icon state logic is unchanged; Android is a no-op.

## Testing

Verified on Linux (AppImage): hovering commands after clicking several in a row, clicking stacked
`/join` commands without moving the mouse, and sweeping across commands quickly — the hand cursor
now tracks correctly in all cases.
