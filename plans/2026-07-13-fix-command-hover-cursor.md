# Fix: hand cursor not appearing over clickable commands (desktop)

## Problem

On desktop, the mouse cursor should change to a hand over clickable commands in chat messages
(e.g. `/join 143556` in a bot's menu of commands) — the hand is what tells the user the command
can be clicked. This did not work reliably: hovering a command sometimes left the text/arrow
cursor in place, giving no indication that the command is clickable. It usually worked, but failed
intermittently — most often when clicking many commands in a row in a chat, because every click
inserts the sent message and shifts the list under the pointer — and once wrong, the cursor stayed
wrong even while moving within the same message; it only recovered after leaving the message text
and re-entering it. The same mechanism affects all clickable elements in message text (links,
simplex addresses, secrets), but command menus made it most visible.

A second user-visible problem surfaced while testing this fix: a click on a command was sometimes
not registered at all when clicking two commands (e.g. two `/join`s) in short succession. That is
defect 3 below — two independent mechanisms in the click path, unrelated to the cursor layers.

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

**Fix (initial):** in `onHover` — which now fires reliably on every Move/Enter/Release over the
text — set the AWT cursor imperatively on desktop (`desktopSetHoverCursor`, writing to the same
Skia canvas component Compose writes to), and symmetrically reset both the cursor and the `icon`
state on the per-node `Exit` event.

**Removed after defect 5 was fixed at the root.** The empirical justification for the imperative
layer (stage 1 under Testing) was contaminated: that test ran while defect 5 was live, i.e. while
every message insertion reset every hover handler in the viewport — which alone explains the
observed failure (a reset suspending handler restarts only on the next pointer event, leaving a
stationary cursor blind after every click). With defects 1 and 5 fixed, the full hover matrix
(rapid stationary-mouse command clicks, list shifting under a hovered pointer, fast sweeps across
commands/links/text, press-drag-out-release-re-enter, wheel-scroll over commands) passes without
the imperative layer, so it was deleted; `Modifier.kt`/`Modifier.desktop.kt`/`Modifier.android.kt`
are untouched by this PR again. What remains of this defect's fix is the `icon.value = Text` reset
on `Exit` in `MarkdownText` — without it, `pointerHoverIcon` re-displays a stale Hand on the next
Enter. The display-layer edges documented above are real in the sources, but with stable handlers
they are not practically reachable in these scenarios.

### Residual limitation (known, strictly smaller than the fixed bug)

If a hovered clickable text leaves the composition without the pointer ever getting an `Exit`
event (e.g. the hovered message is deleted, or a fast fling disposes the item within a frame), the
`onExit` reset does not run — the coroutine is simply cancelled. The retained `pointerHoverIcon`
modifier's `onDetach` reset covers this when its node is in the normal (non-desynced) state; in the
rare desynced state the cursor may stay a hand until the next hover write. This is the same event
class the fix reduces, bounded to one stale frame region, and self-corrects on any subsequent
hover.

### Defect 3: command clicks intermittently lost (commit "multiplatform: fix command clicks lost on quick successive clicks")

Clicking two commands in short succession sometimes lost a click. Two independent mechanisms,
each reproduced in an isolated harness before fixing and re-verified fixed with the same harness:

1. **Press-scope race in `detectGesture`** (GestureDetector.kt — a 2022 copy of
   `detectTapGestures` predating upstream's hardening; the file has not been resynced since).
   The single `PressGestureScopeImpl` is shared across gestures, and its methods run in two
   different lanes of the UI thread: `reset()`/`release()` synchronously inside pointer-event
   dispatch, while the `onPress` handler (which awaits release, then calls `onClick`) runs in a
   launched coroutine resumed through the dispatcher queue. With the old non-suspending
   `reset()` (`mutex.tryLock()`), two fast clicks interleave as: up₁ `release()` sets
   `isReleased` and unlocks — the first click's `tryAwaitRelease` resumption is only *queued* —
   then down₂ `reset()` clears `isReleased` synchronously, so the queued resumption reads
   `false` and the first click's `onClick` never fires. Reproduced deterministically in a
   single-threaded coroutine harness replaying this ordering (`click1=false click2=true`).

   **Fix:** mirror the `detectTapGestures` serialization from Compose 1.8.2 (`foundation`
   `TapGestureDetector.kt`): `reset()` is `suspend` and takes the mutex, so a new gesture cannot
   clear the flags until the previous gesture's press handler finished; `release()`/`cancel()`
   unlock guardedly (`if (mutex.isLocked)`) and are launched joining the reset job, so flag
   writes cannot be reordered across gestures; `tryAwaitRelease()` releases the mutex after
   acquiring it, which is what lets the next `reset()` proceed. The mutex becomes a
   serialization token between consecutive gestures. `detectGesture` also runs for Android
   touch, so the same quick-tap loss is fixed there.

2. **`pointerInput` restart swallowing an in-flight click** (TextItemView.kt). `ClickableText`
   keyed `pointerInput(onClick, onLongClick)` on lambdas that are new instances every
   recomposition, so any recomposition of the message restarted the gesture coroutine and
   destroyed a gesture in flight: the cancelled `onPress` never resumes, and the restarted
   detector waits for a fresh down that never comes for the press already held. Clicking
   command 1 sends a message whose insertion recomposes the item 50–300 ms later — exactly when
   the second click tends to be pressed. Reproduced in a headless `ImageComposeScene` driving
   the verbatim repo gesture code with synthetic pointer events: press → recomposition applies →
   release lost the click, while control clicks on either side registered.

   **Fix:** key both `pointerInput` blocks on `Unit` and read the latest handlers through
   `rememberUpdatedState` (the standard idiom for long-lived event coroutines). This also stops
   recompositions from cancelling `detectCursorMove` mid-stream — the same event-loss class as
   defect 1 — and gives all `ClickableText` callers latest-capture semantics for
   `annotatedText` (previously the handlers could act on a stale capture until the restart).

A theoretical residue remains: if three gestures' pointer events were all processed before the
dispatcher ran any of the second gesture's jobs, flags could still mispair. This exposure is
structurally identical in upstream Compose 1.8.2 (`launchAwaitingReset` joins only its own
gesture's reset job), is not reachable by human input, and fixing it would diverge from the
mirrored upstream — left as upstream parity.

### Defect 4: press cancelled when the list shifts under the pointer (commit "multiplatform: don't cancel command click when chat list shifts under the pointer")

Clicks were still lost after defect 3's fixes. `waitForUpOrCancellation` cancels a press when an
event's position is out of the node's bounds — correct for a pointer dragged away, wrong when the
*node* moved out from under a stationary pointer, which is exactly what happens when the sent
command's message inserts and shifts the list during the press. In node-local coordinates the two
cases are numerically identical; reproduced in a headless harness (60px shift under a held
pointer → synthetic `Exit` at out-of-bounds local position → press cancelled).

**Fix:** the two cases separate in window coordinates. `ClickableText` tracks its
`LayoutCoordinates` (`onGloballyPositioned`) and passes a window-position lambda into
`detectGesture`; `waitForUpOrCancellation` exempts the out-of-bounds cancel when the pointer moved
≤ `touchSlop` in window space since the down. Real drag-aways (> slop) cancel as before; both
parameters default to null, reducing the check literally to the old expression for other callers.

### Defect 5: every message insertion reset all pointer handlers (commit "multiplatform: stop pointer handler resets on chat item recomposition…")

The dominant residual mechanism, found by instrumenting handler lifecycle: every insertion
produced a burst of gesture-handler cancellations for all visible clickable messages — with the
composables *not* disposed and the handler restarting on the *same* node. A press in flight during
such a burst died silently (`down` with no release/cancel), needing a second click; the hover
handler reset the same way, causing the residual hand-cursor flicker.

Cause: `ChatViewListItem` provided `LocalViewConfiguration.current.bigTouchSlop()` — a **new
anonymous `ViewConfiguration` instance on every recomposition** of every item, and
`SuspendingPointerInputModifierNodeImpl.onViewConfigurationChange()` is
`resetPointerInputHandler()` (verified in the 1.8.2 sources). Insertions recompose all visible
items (shifting `index`), so each insertion reset every pointer handler in the viewport.

**Fix:** provide a remembered instance — `remember(viewConfiguration) {
viewConfiguration.bigTouchSlop() }` — keyed on the parent `ViewConfiguration` so a real platform
change still regenerates it. Values are unchanged (the wrapper delegates); only the identity that
the reset machinery reacts to is stabilized. Verified in a harness: a press held across a
recomposition dies with the per-recomposition instance and survives with the remembered one.

## Performance

- `detectCursorMove` is now cheaper per event than before (no scope teardown/re-entry per event);
  the pressed check allocates one list iterator per event.
- Up to two added recompositions per exit/re-enter cycle of a Hand region (the exit reset writes
  `icon.value` Hand→Text, making the next re-enter a Text→Hand write where pre-fix both were
  no-ops); otherwise the icon state logic is unchanged. Android is a no-op.
- The defect 3 fix launches two extra short-lived coroutines per gesture (reset job, release/cancel
  job) — negligible at click rate. Not restarting `pointerInput` on every recomposition removes
  per-recomposition coroutine churn that existed before.

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

Defect 3 is verified by the two isolated repro harnesses described above (both mechanisms
reproduced pre-fix, both register every click post-fix — including a click held across a
recomposition and two fast clicks back-to-back through real Compose event dispatch in
`ImageComposeScene`), by compilation, and by multi-pass adversarial review (two clean passes
pre-commit, two post-commit).

Stage 1's conclusion is retroactively invalidated by defect 5 (see defect 2's removal note): the
"detection fix alone is insufficient" evidence was gathered while insertions were resetting the
detection handler itself. After defect 5's fix, the imperative workaround was removed and the
hover scenarios re-verified passing on the framework path alone.

Defects 4 and 5 were isolated with four rounds of temporary instrumentation in the real app
(logging every gesture exit path, composable disposal, and handler/node identity — removed before
commit), each mechanism reproduced and its fix verified in a headless harness, reviewed through
two clean adversarial passes, and confirmed fixed by runtime testing on Linux: fast successive
command clicks all register, and the residual hand-cursor flicker on message insertion is gone.
Known unfixable-at-this-layer residue: a click that lands where a command *was* before the list
shifted (pre-press shift — an aiming race, not an event-handling defect).
