# Desktop Text Selection Plan

## Goal
Cross-message text selection on desktop (Compose Multiplatform):
1. Click+drag to select message text, with auto-scroll
2. Only message text is selectable (no timestamps, names, quotes, dates — like Telegram web)
3. Ctrl+C and copy button
4. Selection persists across scroll

## Architecture

### Selection State

Selection is two endpoints in the item list:

```kotlin
data class SelectionRange(
    val startIndex: Int,    // anchor — where drag began, immutable during drag
    val startOffset: Int,   // character offset within anchor item
    val endIndex: Int,      // focus — where pointer is now
    val endOffset: Int      // character offset within focus item
)
```

```kotlin
enum class SelectionState { Idle, Selecting, Selected }
```

SelectionManager holds:
```kotlin
var selectionState: SelectionState       // mutableStateOf
var range: SelectionRange?               // mutableStateOf, null in Idle
var focusWindowY by mutableStateOf(0f)   // pointer Y in window coords
var focusWindowX by mutableStateOf(0f)   // pointer X in window coords
```

No captured map. No eager text extraction. No window-coordinate-based range.
Indices are stable across scroll. Text extracted at copy time from live data.

### State Machine

```
         drag threshold
  Idle ─────────────────→ Selecting
   ↑                          │
   │ click                    │ pointer up
   │                          ▼
   ←──────────────────── Selected
```

### What Each Item Has

Each selectable item (CIMarkdownText, EmojiItemView) maintains:
- `boundsState: MutableState<Rect?>` — from `onGloballyPositioned` (added by us)
- `layoutResultState: MutableState<TextLayoutResult?>` — from `onTextLayoutResult` (added by us)
- `contentLength: Int` — computed once during composition from `buildMsgAnnotatedString(...).text.length`

`contentLength` is needed for clamping char offsets in anchor/focus resolution
(preventing offsets from landing in the invisible reserve area).
Highlight range clamping is separate — done inside MarkdownText using its own
`contentAnnotated.text.length`. Computed once per composition, cached in a local val.

### Pointer Handler (on LazyColumn Modifier)

The `SelectionHandler` composable returns a Modifier applied to LazyColumnWithScrollBar.
Contains `pointerInput`, `onGloballyPositioned`, `focusRequester`, `focusable`, `onKeyEvent`.

On every pointer move during Selecting:
1. Updates `focusWindowY/X`
2. Uses `listState.layoutInfo.visibleItemsInfo` to find item at pointer Y → updates `range.endIndex`

Index resolution uses LazyListState directly — no map, no registration.

### Anchor Char Offset Resolution

The anchor item knows it's the anchor: `range.startIndex == myIndex`.
Resolves char offset ONCE at selection start via LaunchedEffect:

```kotlin
val isAnchor = remember(myIndex) {
    derivedStateOf { manager.range?.startIndex == myIndex && manager.selectionState == SelectionState.Selecting }
}
LaunchedEffect(isAnchor.value) {
    if (!isAnchor.value) return@LaunchedEffect
    val bounds = boundsState.value ?: return@LaunchedEffect
    val layout = layoutResultState.value ?: return@LaunchedEffect
    val offset = layout.getOffsetForPosition(
        Offset(manager.focusWindowX - bounds.left, manager.focusWindowY - bounds.top)
    )
    manager.setAnchorOffset(offset.coerceAtMost(contentLength))
}
```

Fires once. No ongoing effect.

### Focus Char Offset Resolution

The focus item knows it's the focus: `range.endIndex == myIndex`.
Resolves char offset on every pointer move via snapshotFlow:

```kotlin
val isFocus = remember(myIndex) {
    derivedStateOf { manager.range?.endIndex == myIndex && manager.selectionState == SelectionState.Selecting }
}
if (isFocus.value) {
    LaunchedEffect(Unit) {
        snapshotFlow { manager.focusWindowY to manager.focusWindowX }
            .collect { (py, px) ->
                val bounds = boundsState.value ?: return@collect
                val layout = layoutResultState.value ?: return@collect
                val offset = layout.getOffsetForPosition(Offset(px - bounds.left, py - bounds.top))
                manager.updateFocusOffset(offset.coerceAtMost(contentLength))
            }
    }
}
```

- Starts when item becomes focus, cancels when focus moves to different item
- snapshotFlow fires on pointer move, but only in ONE item
- Uses item's own local TextLayoutResult — no shared map

### Highlight Rendering (Per Item)

Each item computes highlight via derivedStateOf:

```kotlin
val highlightRange = remember(myIndex) {
    derivedStateOf { manager.computeHighlightRange(myIndex) }
}
```

`computeHighlightRange` logic:
```kotlin
fun computeHighlightRange(index: Int): IntRange? {
    val r = range ?: return null
    val lo = minOf(r.startIndex, r.endIndex)
    val hi = maxOf(r.startIndex, r.endIndex)
    if (index < lo || index > hi) return null
    val forward = r.startIndex <= r.endIndex
    val startOff = if (forward) r.startOffset else r.endOffset
    val endOff = if (forward) r.endOffset else r.startOffset
    return when {
        index == lo && index == hi -> minOf(startOff, endOff) until maxOf(startOff, endOff)
        index == lo -> startOff until Int.MAX_VALUE   // clamped by MarkdownText to content length
        index == hi -> 0 until endOff
        else -> 0 until Int.MAX_VALUE                 // fully selected, clamped by MarkdownText
    }
}
```

derivedStateOf only triggers recomposition when the RESULT changes for this item.
Middle items don't recompose as range extends. Only boundary items recompose.
MarkdownText clamps open-ended ranges to `contentAnnotated.text.length` internally.

### Highlight Drawing

Same as current: `getPathForRange(range.first, range.last + 1)` in `drawBehind`
on BasicText. Off-by-one fixed: `range.last + 1` because IntRange.last is inclusive,
getPathForRange end is exclusive.

### Copy

#### Extracted function: `buildMsgAnnotatedString`

The `buildAnnotatedString` block inside MarkdownText is extracted into a standalone
function, placed right next to MarkdownText in TextItemView.kt. The extraction is
surgical — the code body is moved verbatim, all variables it reads from scope become
parameters with the SAME names, so the diff shows no code changes inside the block.

```kotlin
fun buildMsgAnnotatedString(
    text: CharSequence,
    formattedText: List<FormattedText>?,
    sender: String?,
    senderBold: Boolean,
    prefix: AnnotatedString?,
    mentions: Map<String, CIMention>?,
    userMemberId: String?,
    toggleSecrets: Boolean,
    sendCommandMsg: Boolean,
    linkMode: SimplexLinkMode
): AnnotatedString = buildAnnotatedString {
    appendSender(this, sender, senderBold)
    if (prefix != null) append(prefix)
    if (formattedText == null) {
        if (text is String) append(text)
        else if (text is AnnotatedString) append(text)
    } else {
        // Exact same formatted text loop as current MarkdownText,
        // moved verbatim. Handles all Format types.
    }
}
```

Handles BOTH paths (formattedText null and non-null) in one function.

EXCLUDES: `inlineContent`, typing indicator, reserve, `showSecrets` state.
- `showSecrets` is local composition state (which secrets user revealed).
  Not passed — secrets render as hidden in the extracted function.
  MarkdownText overrides this for rendering by passing its local `showSecrets`.
  For copy, hidden secrets are copied as-is (privacy-safe default).

MarkdownText calls it, then wraps with rendering concerns:
```kotlin
val contentAnnotated = buildMsgAnnotatedString(text, formattedText, sender, ...)
val contentLength = contentAnnotated.text.length
val fullAnnotated = buildAnnotatedString {
    append(contentAnnotated)
    if (meta?.isLive == true) { append(typingIndicator(...)) }
    if (meta != null) withStyle(reserveTimestampStyle) { append(reserve) }
}
// Clamp selectionRange to contentLength before passing to drawBehind
```

For rendering with revealed secrets: MarkdownText builds the secret spans
differently (using its local showSecrets map). This means the rendering path
cannot simply reuse the extracted function as-is for the secret case — it
needs to override the secret handling. Options:
a) Pass `showSecrets` as parameter (default empty for copy, actual map for rendering)
b) MarkdownText post-processes the result to reveal secrets
c) Accept that rendering still builds its own AnnotatedString for the secret case

Option (a) is cleanest — add `showSecrets: Map<String, Boolean> = emptyMap()`
as parameter with empty default. Rendering passes the actual map. Copy passes nothing.

Updated signature:
```kotlin
fun buildMsgAnnotatedString(
    text: CharSequence,
    formattedText: List<FormattedText>?,
    sender: String?,
    senderBold: Boolean,
    prefix: AnnotatedString?,
    mentions: Map<String, CIMention>?,
    userMemberId: String?,
    toggleSecrets: Boolean,
    showSecrets: Map<String, Boolean> = emptyMap(),
    sendCommandMsg: Boolean,
    linkMode: SimplexLinkMode
): AnnotatedString
```

#### Copy text extraction

At copy time, call `buildMsgAnnotatedString` for each item and take `.text`:

```kotlin
fun getSelectedText(items: List<MergedItem>): String {
    val r = range ?: return ""
    val lo = minOf(r.startIndex, r.endIndex)
    val hi = maxOf(r.startIndex, r.endIndex)
    val forward = r.startIndex <= r.endIndex
    val startOff = if (forward) r.startOffset else r.endOffset
    val endOff = if (forward) r.endOffset else r.startOffset
    return (lo..hi).mapNotNull { idx ->
        val text = items.getOrNull(idx)?.getDisplayText() ?: return@mapNotNull null
        when {
            idx == lo && idx == hi -> text.substring(
                startOff.coerceAtMost(text.length),
                endOff.coerceAtMost(text.length)
            )
            idx == lo -> text.substring(startOff.coerceAtMost(text.length))
            idx == hi -> text.substring(0, endOff.coerceAtMost(text.length))
            else -> text
        }
    }.joinToString("\n")
}
```

Where `getDisplayText()` calls `buildMsgAnnotatedString(...).text` with the
item's ChatItem fields. One function, used for both rendering and copy.
No stored text state. No map. No `annotatedTextState`.

Chat-level parameters for copy (`linkMode`, `userMemberId`, `sendCommandMsg` flag)
are passed to `getSelectedText` from the call site (SelectionHandler), which has
access to chat context via the composable scope.

### Pointer Handler Behavior Per State

Non-press events (hover, scroll) skipped: `return@awaitEachGesture`.
State captured at gesture start (`wasSelected`).

**Idle**: Down not consumed. Links/menus work. Drag threshold → Selecting.
**Selecting**: Pointer move → update focusWindowY/X, resolve endIndex via listState.
  Pointer up → Selected.
**Selected**: Down consumed (prevents link activation). Click → Idle. Drag → new Selecting.

### Auto-Scroll

Direction-aware: only the edge you're dragging toward.
After `scrollBy()`, re-resolve index from `listState.layoutInfo.visibleItemsInfo`
with same pointer Y. Different item may be under pointer → endIndex updates.
Indices don't shift on scroll. Focus item's snapshotFlow handles new charOffset.

### Mouse Wheel During Drag

Scroll event passes through to LazyColumn (not consumed by handler).
`snapshotFlow` on scroll offset fires → re-resolve index from listState → update endIndex.

### Ctrl+C / Cmd+C

`onKeyEvent` on LazyColumn modifier. Focus requested on selection start.
Checks `isCtrlPressed || isMetaPressed`. Extracts text from live data at copy time.

### Copy Button

Emitted by SelectionHandler in BoxScope. Visible in Selected state.
Copies without clearing. Click in chat clears selection.

### Reserve Space Exclusion

MarkdownText calls `buildMsgAnnotatedString` for content, then appends typing indicator
and reserve. It knows the content length (`contentAnnotated.text.length`) and clamps
`selectionRange` internally before passing to drawBehind. No `selectableEnd` parameter
needed — the information stays inside MarkdownText.

### Eviction Prevention

`ChatItemsLoader.kt`: `allowedTrimming = !selectionActive` during selection.

### Platform Gate

All selection code gated on `appPlatform.isDesktop`.

### Swipe-to-Reply

Disabled on desktop: `if (appPlatform.isDesktop) Modifier else swipeableModifier`.

### RTL Text

`getOffsetForPosition` and `getPathForRange` are bidi-aware. No direction assumptions.

---

## Effects Summary

### Idle State
Zero effects. Items don't check anything. `range` is null.

### Selecting State

| What | Scope | Fires when |
|------|-------|-----------|
| Pointer event handling | LazyColumn pointerInput (total: 1) | Every pointer event |
| Index resolution | Pointer handler via listState (total: 1) | Every pointer move + scroll |
| Anchor char offset | Anchor item LaunchedEffect (1 item) | Once at selection start |
| Focus char offset | Focus item snapshotFlow (1 item) | Every pointer move |
| Highlight derivedStateOf | Per item (passive) | Only when result changes (~2 items) |
| Auto-scroll | Coroutine in pointer handler (total: 0 or 1) | Near edge during drag |
| Scroll re-evaluation | snapshotFlow on scroll offset (total: 1) | On scroll during drag |

### Selected State
Zero effects. Frozen range. Items render highlight from derivedStateOf (no recomposition
unless range changes, which it doesn't in Selected state).

---

## Changes From Current State

The current implementation has: SelectionManager with captured map, SelectionParticipant
interface, coordinate-based recomputeAll, annotatedTextState, selectableEnd MutableIntState.
Below describes what changes in each file to reach the target design.

### TextItemView.kt

**Extract `buildMsgAnnotatedString`** (new function, right next to MarkdownText):
- Move the `buildAnnotatedString` body from MarkdownText into standalone function
- Surgical extraction: code verbatim, scope variables become parameters with SAME names
- EXCLUDES: inlineContent, typing indicator, reserve — those stay in MarkdownText
- Returns AnnotatedString (not String) — reused for both rendering and copy

**MarkdownText parameter changes** (remove 2, keep 2 of the 4 we added):
- Remove `selectableEnd: MutableIntState?` — clamping now internal
- Remove `annotatedTextState: MutableState<String>?` — copy uses extracted function
- Keep `selectionRange: IntRange?` — needed for highlight
- Keep `onTextLayoutResult: ((TextLayoutResult) -> Unit)?` — needed for focus char offset

**MarkdownText internal change**:
- Call `buildMsgAnnotatedString(...)` to get content AnnotatedString
- Wrap with typing indicator + reserve: `buildAnnotatedString { append(contentAnnotated); ... }`
- Compute `contentLength = contentAnnotated.text.length`
- Clamp `selectionRange` to `0..contentLength` before passing to drawBehind
- Remove `selectableEnd?.intValue = this.length` (2 places)
- Remove `annotatedTextState?.value = annotatedText.text` (2 places)
- For rendering with secrets: pass local `showSecrets` map to `buildMsgAnnotatedString`

**SelectableText, ClickableText highlight** — unchanged (already correct with `range.last + 1`)

### TextSelection.kt — rewrite

**Remove entirely:**
- `SelectionCoords` data class
- `CapturedText` data class
- `SelectionParticipant` interface
- `captured` map
- `recomputeAll()`, `recomputeParticipant()`
- `calculateRangeForElement()` function
- `getHighlightRange(itemId)` — replaced by `computeHighlightRange(index)`

**Keep (already correct):**
- `SelectionState` enum (Idle, Selecting, Selected)
- `SelectionCopyButton` composable
- `SelectionHandler` composable structure (returns Modifier, emits copy button)
- Pointer handler state machine (Idle/Selecting/Selected behavior)
- Auto-scroll logic
- Scroll snapshotFlow re-evaluation

**Add/replace:**
- `SelectionRange(startIndex, startOffset, endIndex, endOffset)` data class
- `selectionState: SelectionState` — mutableStateOf (already exists)
- `range: SelectionRange?` — mutableStateOf, replaces `coords`
- `focusWindowY/X` — mutableStateOf (were plain Floats, need observable for snapshotFlow)
- `computeHighlightRange(index): IntRange?` — returns range or null, uses Int.MAX_VALUE for open ends
- `getSelectedText(items)` — calls `buildMsgAnnotatedString(...).text` per item at copy time
- `startSelection(startIndex)`, `updateFocusIndex(index)`, `setAnchorOffset(offset)`,
  `updateFocusOffset(offset)`, `endSelection()`, `clearSelection()`

**SelectionHandler changes:**
- Pointer move: update `focusWindowY/X`, resolve index via `listState.layoutInfo.visibleItemsInfo`
- Remove `recomputeAll` / `updateSelection` calls — replaced by `updateFocusIndex`
- Auto-scroll: after `scrollBy`, re-resolve index via listState
- Scroll snapshotFlow: re-resolve index via listState
- Copy button: call `getSelectedText(items)` at click time

### FramedItemView.kt — CIMarkdownText

**Remove:**
- `annotatedTextState` and its `remember`
- `selectableEnd` MutableIntState and its `remember`
- `SelectionParticipant` anonymous object
- `DisposableEffect` for register/unregister
- `selectionManager?.getHighlightRange(ci.id)` call

**Keep:**
- `selectionManager = LocalSelectionManager.current`
- `boundsState` — needed for focus/anchor char offset resolution
- `layoutResultState` — needed for focus/anchor char offset resolution

**Add:**
- Item index parameter (passed from ChatView)
- `contentLength` — computed once: `buildMsgAnnotatedString(text, ci.formattedText, ...).text.length`
  Used to clamp anchor/focus char offsets. Recomputed only when item recomposes.
- `highlightRange` via `derivedStateOf { manager.computeHighlightRange(myIndex) }`
- `isAnchor` derivedStateOf + LaunchedEffect (resolves anchor offset once, clamps to contentLength)
- `isFocus` derivedStateOf + LaunchedEffect with snapshotFlow (resolves focus offset, clamps to contentLength)

**MarkdownText call changes:**
- Remove `selectableEnd = selectableEnd`
- Remove `annotatedTextState = annotatedTextState`
- Keep `selectionRange = highlightRange` (source changes from getHighlightRange to derivedStateOf)
- Keep `onTextLayoutResult = { layoutResultState.value = it }`

### EmojiItemView.kt

**Remove:**
- `SelectionParticipant` anonymous object
- `DisposableEffect` for register/unregister
- `currentEmojiText` rememberUpdatedState
- `selectionManager?.getHighlightRange(chatItem.id)` call

**Keep:**
- `selectionManager = LocalSelectionManager.current`
- `boundsState` — for focus/anchor resolution

**Add:**
- Item index parameter
- `isSelected` via `derivedStateOf { manager.computeHighlightRange(myIndex) != null }`
- `isAnchor`/`isFocus` effects (same pattern as CIMarkdownText, but full-selection only)

### ChatView.kt

**Beyond current diff:**
- Pass item index from `itemsIndexed` through to CIMarkdownText and EmojiItemView
- The index passes through: `ChatViewListItem` → `ChatItemViewShortHand` → `ChatItemView`
  (item/ChatItemView.kt) → `FramedItemView` → `CIMarkdownText`. Each function gets
  a new `selectionIndex: Int` parameter. This is 4-5 function signatures changed,
  but each change is one line (add parameter, pass through).
- Pass merged items list reference to SelectionHandler for copy text extraction
- Pass chat context (linkMode, userMemberId) to SelectionHandler for copy

### ChatItemsLoader.kt, ChatItemsMerger.kt — no change

Already correct: `selectionActive` field and `allowedTrimming` gating.

---

## Testing

1. Single message partial character selection
2. Multi-message selection with highlights
3. Direction reversal past anchor
4. Selection shrinks on reverse (items unhighlight)
5. Selection persists after drag end and across scroll
6. Auto-scroll extends selection correctly
7. Auto-scroll loads items from DB
8. Mouse wheel during drag extends selection
9. Items scrolling out and back in retain highlight
10. Click on links works (Idle state)
11. Click in chat clears selection (Selected state)
12. Right-click behavior
13. Ctrl+C / Cmd+C copies selected text
14. Copy button works
15. Highlight stops before invisible reserve space
16. Copy produces clean text
17. RTL text
18. Emoji-only messages
19. Live messages excluded
20. Edited messages during selection
