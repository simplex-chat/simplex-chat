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

No captured map. No eager text extraction.
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

### Pointer Handler (on LazyColumn Modifier)

`SelectionHandler` composable (BoxScope extension) returns a Modifier for
LazyColumnWithScrollBar. Contains `pointerInput`, `onGloballyPositioned`,
`focusRequester`, `focusable`, `onKeyEvent`.

On every pointer move during Selecting:
1. Updates `focusWindowY/X`
2. Uses `listState.layoutInfo.visibleItemsInfo` to find item at pointer Y → updates `range.endIndex`

Index resolution uses LazyListState directly — no map, no registration.

### Pointer Handler Behavior Per State

Non-press events (hover, scroll) skipped: `return@awaitEachGesture`.
State captured at gesture start (`wasSelected`).

**Idle**: Down not consumed. Links/menus work. Drag threshold → Selecting.
**Selecting**: Pointer move → update focusWindowY/X, resolve endIndex via listState.
  Pointer up → Selected.
**Selected**: Down consumed (prevents link activation). Click → Idle. Drag → new Selecting.

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
    manager.setAnchorOffset(offset)
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
                manager.updateFocusOffset(offset)
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
    derivedStateOf { highlightedRange(manager.range, myIndex) }
}
```

`highlightedRange` is a standalone function:
```kotlin
fun highlightedRange(range: SelectionRange?, index: Int): IntRange? {
    val r = range ?: return null
    val lo = minOf(r.startIndex, r.endIndex)
    val hi = maxOf(r.startIndex, r.endIndex)
    if (index < lo || index > hi) return null
    val forward = r.startIndex <= r.endIndex
    val startOff = if (forward) r.startOffset else r.endOffset
    val endOff = if (forward) r.endOffset else r.startOffset
    return when {
        index == lo && index == hi -> minOf(startOff, endOff) until maxOf(startOff, endOff)
        index == lo -> startOff until Int.MAX_VALUE   // clamped by MarkdownText
        index == hi -> 0 until endOff
        else -> 0 until Int.MAX_VALUE                 // clamped by MarkdownText
    }
}
```

derivedStateOf only triggers recomposition when the RESULT changes for this item.
Middle items don't recompose as range extends. Only boundary items recompose.

### Highlight Drawing

`getPathForRange(range.first, range.last + 1)` in `drawBehind` on BasicText.
`range.last + 1` because IntRange.last is inclusive, getPathForRange end is exclusive.

Gated on `selectionRange != null`:
- When null (Android, or desktop without selection): original `Text()` used, no drawBehind.
- When non-null: `SelectableText` (BasicText + drawBehind + onTextLayout) or
  `ClickableText` with added drawBehind.

### Reserve Space Exclusion

MarkdownText's `buildAnnotatedString` appends invisible reserve text after message
content. A local `var selectableEnd` is set to `this.length` inside `buildAnnotatedString`
right before reserve is appended. Used to clamp `selectionRange` before passing
downstream to rendering:

```kotlin
var selectableEnd = 0
val annotatedText = buildAnnotatedString {
    // ... content ...
    selectableEnd = this.length
    // ... typing indicator, reserve ...
}
val clampedRange = selectionRange?.let { it.first until minOf(it.last, selectableEnd) }
// pass clampedRange to ClickableText/SelectableText
```

`selectableEnd` is local to MarkdownText. Not passed upstream.
`highlightedRange` uses `Int.MAX_VALUE` for open-ended ranges;
MarkdownText resolves them to the actual content boundary.

### Copy

#### `displayText` function

Non-composable function placed right next to MarkdownText in TextItemView.kt.
Computes the displayed text from `formattedText`, handling only the few Format
types that change the displayed string. All other formats use `ft.text` unchanged.
Used only at copy time.

```kotlin
// Must be coordinated with MarkdownText — same text transformations for:
// Mention, HyperLink, SimplexLink, Command
fun displayText(
    ci: ChatItem,
    linkMode: SimplexLinkMode,
    sendCommandMsg: Boolean
): String {
    val formattedText = ci.formattedText
    if (formattedText == null) return ci.text
    return formattedText.joinToString("") { ft ->
        when (ft.format) {
            is Format.Mention -> { /* resolve display name from ci.mentions */ }
            is Format.HyperLink -> ft.format.showText ?: ft.text
            is Format.SimplexLink -> { /* showText or description + viaHosts */ }
            is Format.Command -> if (sendCommandMsg) "/${ft.format.commandStr}" else ft.text
            else -> ft.text
        }
    }
}
```

MarkdownText gets a corresponding comment noting these transformations must match.

#### Copy text extraction

On SelectionManager:
```kotlin
fun getSelectedText(items: List<MergedItem>, linkMode: SimplexLinkMode): String {
    val r = range ?: return ""
    val lo = minOf(r.startIndex, r.endIndex)
    val hi = maxOf(r.startIndex, r.endIndex)
    val forward = r.startIndex <= r.endIndex
    val startOff = if (forward) r.startOffset else r.endOffset
    val endOff = if (forward) r.endOffset else r.startOffset
    return (lo..hi).mapNotNull { idx ->
        val ci = items.getOrNull(idx)?.newest()?.item ?: return@mapNotNull null
        val text = displayText(ci, linkMode, sendCommandMsg = false)
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

### Auto-Scroll

Direction-aware: only the edge you're dragging toward.
After `scrollBy()`, re-resolve index from `listState.layoutInfo.visibleItemsInfo`
with same pointer Y. Different item may be under pointer → endIndex updates.
Indices don't shift on scroll. Focus item's snapshotFlow handles new charOffset.

### Mouse Wheel During Drag

Scroll event passes through to LazyColumn (not consumed by handler).
`snapshotFlow` on scroll offset fires → re-resolve index from listState → update endIndex.

### Ctrl+C / Cmd+C

`onKeyEvent` on LazyColumn modifier (inside SelectionHandler's returned Modifier).
Focus requested on selection start. When user taps compose box, focus moves there —
Ctrl+C goes to compose box handler. Copy button works regardless of focus.
Checks `isCtrlPressed || isMetaPressed`.

### Copy Button

Emitted by SelectionHandler in BoxScope. Visible in Selected state.
Copies without clearing. Click in chat clears selection.

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

## Changes From Master

### NEW: TextSelection.kt

New file: `common/src/commonMain/kotlin/chat/simplex/common/views/chat/TextSelection.kt`

Contains:
- `SelectionRange(startIndex, startOffset, endIndex, endOffset)` data class
- `SelectionState` enum (Idle, Selecting, Selected)
- `SelectionManager` — holds `selectionState`, `range`, `focusWindowY/X` (mutableStateOf),
  methods: `startSelection`, `setAnchorOffset`, `updateFocusIndex`, `updateFocusOffset`,
  `endSelection`, `clearSelection`, `getSelectedText(items, linkMode)`
- `highlightedRange(range, index)` standalone function
- `LocalSelectionManager` CompositionLocal
- `SelectionHandler` composable (BoxScope extension, returns Modifier for LazyColumn):
  pointer input with state machine, auto-scroll, focus management, Ctrl+C/Cmd+C, copy button
- `SelectionCopyButton` composable
- `resolveIndexAtY` helper for pointer → item index via listState

### TextItemView.kt

**Add `displayText` function** right next to MarkdownText, with comment that it
must be coordinated with MarkdownText's text transformations. Takes `ChatItem`,
`linkMode`, `sendCommandMsg`. Used only by `getSelectedText` at copy time.

**Add comment to MarkdownText** noting `displayText` must match its text transformations.

**Add 2 parameters to MarkdownText**:
- `selectionRange: IntRange? = null`
- `onTextLayoutResult: ((TextLayoutResult) -> Unit)? = null`

**Inside MarkdownText** — local `var selectableEnd` set in both `buildAnnotatedString`
blocks (1 line each, right before typing indicator / reserve). Clamp selectionRange:
```kotlin
val clampedRange = selectionRange?.let { it.first until minOf(it.last, selectableEnd) }
```

**Rendering** — gated on `clampedRange != null`:
- `Text()` call sites (2): `if (clampedRange != null) SelectableText(...) else Text(...)`
  Original `Text(...)` call unchanged.
- `ClickableText` call: add `selectionRange = clampedRange`,
  add `onTextLayout = { onTextLayoutResult?.invoke(it) }`

**Add `selectionRange` parameter to `ClickableText`**, add `drawBehind` highlight
with `getPathForRange(range.first, range.last + 1)` before BasicText.

**Add `SelectableText` private composable** — BasicText + drawBehind highlight +
onTextLayout. Used only when `selectionRange != null`. On Android, never reached.

**MarkdownText is NOT restructured.** No code moved, no branches regrouped.

### FramedItemView.kt — CIMarkdownText

**Add `selectionIndex: Int = -1` parameter.**

**Add** (gated on `selectionManager != null && selectionIndex >= 0 && !ci.meta.isLive`):
- `boundsState: MutableState<Rect?>` — from `onGloballyPositioned` on the Box
- `layoutResultState: MutableState<TextLayoutResult?>` — from `onTextLayoutResult`
- `isAnchor` derivedStateOf + LaunchedEffect (resolves anchor offset once)
- `isFocus` derivedStateOf + LaunchedEffect with snapshotFlow (resolves focus offset)
- `highlightRange` via `derivedStateOf { highlightedRange(manager.range, selectionIndex) }`

**MarkdownText call**: add `selectionRange = highlightRange`,
`onTextLayoutResult = { layoutResultState.value = it }`

### EmojiItemView.kt

**Add `selectionIndex: Int = -1` parameter.**

**Add** (gated on `selectionManager != null && selectionIndex >= 0`):
- `isAnchor`/`isFocus` LaunchedEffects (full-selection only: offset 0 / emojiText.length)
- `isSelected` via `derivedStateOf { highlightedRange(manager.range, selectionIndex) != null }`
- Highlight via `Modifier.background(SelectionHighlightColor)` when selected

### ChatView.kt

- Create `SelectionManager`, provide via `LocalSelectionManager`
- `SelectionHandler` returns Modifier applied to LazyColumnWithScrollBar
- Pass `selectionIndex` from `itemsIndexed` through the call chain:
  `ChatViewListItem` → `ChatItemViewShortHand` → `ChatItemView` (item/) →
  `FramedItemView` → `CIMarkdownText`. Each gets `selectionIndex: Int = -1` param.
- Same for EmojiItemView path
- Gate SwipeToDismiss on desktop: `if (appPlatform.isDesktop) Modifier else swipeableModifier`
- Sync `selectionState != Idle` to `chatState.selectionActive` via LaunchedEffect

### ChatItemsLoader.kt

- `removeDuplicatesAndModifySplitsOnBeforePagination`: add `selectionActive: Boolean = false` param
- `allowedTrimming = !selectionActive`
- Call site passes `chatState.selectionActive`

### ChatItemsMerger.kt

- `ActiveChatState`: add `@Volatile var selectionActive: Boolean = false`

### ChatModel.kt — no change

### MarkdownHelpView.kt — no change

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
