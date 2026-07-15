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

**Fix:** one never-exiting `awaitPointerEventScope` loop reacting to `Move`, `Enter` and `Release`
(plus an optional `onExit` callback used by the cursor fix below). `Release` refreshes hover at
the final pointer position — after clicking a command the list shifts and the release is the only
event a stationary pointer gets. Button-held events are ignored (gated on *no* pointer pressed,
matching the old `forEachGesture` semantics which waited for all pointers up between events), and
so are out-of-bounds positions: while a button is pressed the hit path is locked, so the pressed
node keeps receiving events after the pointer leaves it — acting on those would apply hover
effects at clamped positions (e.g. a wrong cursor stuck after drag-releasing outside the text).
The other `detectCursorMove` call sites (scrollbar reveal in `ScrollableColumn.desktop.kt` and
`OperatorView.desktop.kt`) were audited: reacting to `Enter`/`Release` there is harmless or an
improvement, and `onExit` defaults to a no-op for them.

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

**Fix:** in `onHover` — which now fires reliably on every Move/Enter/Release over the text — set
the AWT cursor imperatively on desktop (`desktopSetHoverCursor`), and symmetrically reset both the
cursor and the `icon` state on the per-node `Exit` event (delivered reliably to the `pointerInput`
node even when the hover-icon node is desynced and its own exit reset is a guarded no-op; the state
reset prevents the hover-icon node from re-displaying a stale Hand on its next Enter). The set and
reset live together in `MarkdownText` (`onHover`/`onHoverExit`), so other `ClickableText` callers
are untouched. The existing `pointerHoverIcon` modifier is kept, so the normal framework path still
works when its edges fire — and it remains the only path covering disposal-without-Exit and Android
mice. The imperative write targets the same Skia canvas component Compose writes to
(`ComposeSceneMediator.setPointerIcon` → `contentComponent.cursor`), so last-write-wins stays
consistent with the framework's own updates; the canvas lookup is cached weakly (including negative
results, with a one-time warning log if the component is not found after a Compose upgrade — the
workaround then degrades to the framework-only path). No framework edge can leave the cursor
stuck, because it is re-asserted on every mouse move over clickable message text and released on
exit — with the one exception described under "Residual limitation" below.

The same lost-edge class also applies in principle to static `pointerHoverIcon` sites whose content
shifts under a stationary cursor (e.g. the chat list link previews using
`desktopPointerHoverIconHand`); those are low-exposure and left on the framework path — if a
second report arrives, extract a shared reliable-hover-icon modifier from this wiring.

### Residual limitation (known, strictly smaller than the fixed bug)

If a hovered clickable text leaves the composition without the pointer ever getting an `Exit`
event (e.g. the hovered message is deleted, or a fast fling disposes the item within a frame), the
`onExit` reset does not run — the coroutine is simply cancelled. The retained `pointerHoverIcon`
modifier's `onDetach` reset covers this when its node is in the normal (non-desynced) state; in the
rare desynced state the cursor may stay a hand until the next hover write. This is the same event
class the fix reduces, bounded to one stale frame region, and self-corrects on any subsequent
hover.

## Performance

- `detectCursorMove` is now cheaper per event than before (no scope teardown/re-entry per event);
  the pressed check allocates one list iterator per event.
- `desktopSetHoverCursor` runs at mouse-move rate: steady-state cost is a cached component
  reference check (parent-chain walk, no allocations) plus an int comparison; the native
  `setCursor` call fires only when the cursor type actually changes. The canvas component lookup
  (recursive tree walk) runs once per window and is cached, including negative results (a miss is
  unreachable in practice — pointer events originate from the rendered canvas — kept as a guard).
- Up to two added recompositions per exit/re-enter cycle of a Hand region (the exit reset writes
  `icon.value` Hand→Text, making the next re-enter a Text→Hand write where pre-fix both were
  no-ops); otherwise the icon state logic is unchanged. Android is a no-op.

Framework line numbers cited above are from the official `ui-desktop-1.8.2`/`foundation-desktop-1.8.2`
sources jars on Maven Central; they will drift on upgrade.

## Testing

Verified on Linux (AppImage), in two stages (commit subjects name the defect layer each commit
fixes; the user-visible symptom needed both layers, per stage 1 below):

1. A build with only the detection-layer fix (lossless `detectCursorMove`) was tested first and
   the stale-cursor symptom **still reproduced** — this is the empirical justification for the
   imperative display-layer workaround; the declarative-only fix is not sufficient.
2. With both layers (pre-hardening build, commit `df6b0655d`): hovering commands after clicking
   several in a row, clicking stacked `/join` commands without moving the mouse, and sweeping
   across commands quickly — the hand cursor tracks correctly in all cases.

The later hardening commits (release refresh, pressed/bounds gating, exit resets, canvas cache)
are verified by compilation on both targets and multi-pass adversarial review; runtime
re-verification of the final build is pending.
