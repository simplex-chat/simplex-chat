# Desktop Text Selection Plan

## Goal
Cross-message text selection on desktop (Compose Multiplatform):
1. Click+drag to select message text, with auto-scroll
2. Only message text is selectable (no timestamps, names, quotes, dates — like Telegram web)
3. Ctrl+C and copy button
4. Selection persists across scroll

## Architecture

### Selection State: Continuously Resolved Map

Selection state is always a resolved map of `itemId → CapturedText`.
Recomputed on every pointer move and every scroll event.

```kotlin
val captured: MutableStateMap<Long, CapturedText>
```

Each entry has:
- `highlightRange: IntRange` — in AnnotatedString space, for `getPathForRange()`
- `text: String` — clean message text substring, for copy

Window coordinates (`SelectionCoords`) are ephemeral input for computing the map.
Rendering reads the map directly — no coordinate comparison during render.

On every `updateSelection()`:
- Visible participants in range → add/update
- Visible participants out of range → remove
- Scrolled-out participants → keep existing capture

### Participant Registration

Each selectable text composable registers a `SelectionParticipant` on composition
and unregisters on disposal.

Registration sites (2 total):
- `CIMarkdownText` in FramedItemView.kt — covers all text messages, voice-with-text,
  forwarded, quoted, live messages excluded
- `EmojiItemView` — emoji-only messages (desktop implementation is plain `Text()`,
  line 20 of EmojiItemView.desktop.kt; heart emoji renders as `Image` — skip selection
  for that special case only)

NOT registration sites (verified):
- `CIVoiceView` — only called when `cItem.content.text.isEmpty()` (ChatItemView.kt:575).
  Voice messages with text go through `framedItemView()` → CIMarkdownText.

### Reserve Space Exclusion

MarkdownText appends invisible reserve text (transparent timestamp + status icons)
at the end of the AnnotatedString for layout spacing. This must NOT be selectable.

**Minimal change**: MarkdownText receives a `MutableIntState` and writes the content
end offset before appending reserve text:

```kotlin
// Inside buildAnnotatedString, after all content spans:
selectableEnd?.intValue = this.length  // ONE LINE

if (meta?.isLive == true) { append(typingIndicator(...)) }
if (meta != null) withStyle(reserveTimestampStyle) { append(reserve) }
```

The participant clamps all offsets to `0..selectableEnd`. Highlight stops before
invisible reserve. Copy text excludes it.

### AnnotatedString Access

The participant needs the AnnotatedString's plain text for accurate substring extraction
(because mentions may change display length vs ci.text). MarkdownText writes it to a
shared state AFTER `buildAnnotatedString` returns:

```kotlin
// New parameter in MarkdownText:
annotatedTextState: MutableState<String>? = null

// AFTER buildAnnotatedString:
val annotatedText = buildAnnotatedString { ... }
annotatedTextState?.value = annotatedText.text  // .text is documented AnnotatedString API
```

The participant reads `annotatedTextState.value.substring(clampedStart, clampedEnd)` for copy.
Fallback: `ci.text` if state is empty.

### Overlay

Overlay sits on top of LazyColumnWithScrollBar. Uses `PointerEventPass.Initial`
to observe pointer events without consuming. Only consumes after drag threshold
(differentiates click from drag).

Scroll wheel events are NEVER consumed — pass through to LazyColumn.

### Selection Lifecycle

- **Click without drag**: Does nothing. Selection is NOT cleared. Links work via pass-through.
- **New drag**: Clears any existing selection, starts new one (`startSelection` calls `captured.clear()`).
- **Right-click**: Does nothing to selection. `contextMenuOpenDetector` handles it independently.
  No need to detect mouse button — we simply never clear on non-drag pointer-up.
- **Ctrl+C**: Copies selected text to clipboard.
- **Copy button**: Appears after drag ends when `captured.isNotEmpty()`. Has explicit dismiss.
- **Dismiss**: Copy button dismiss clears selection. Or starting a new drag clears it.

This avoids needing `PointerEvent.button` API (not used in codebase, availability uncertain).

### Coordinate System

All coordinates in window space during drag computation:
- Overlay: `positionInWindow()` + local pointer → window coords
- Items: `boundsInWindow()` → window coords
- `calculateRangeForElement` adjusts X by `bounds.left` for `getOffsetForPosition`

Consistent within a frame. After scroll, items have new `boundsInWindow()`,
so `updateSelection()` with same pointer position produces correct new results.

### Bidirectional Drag (Core Design)

Users drag any direction, reverse past anchor, shrink and grow selection.

- `startY/startX` = anchor (click point, never changes)
- `endY/endX` = current pointer
- `topY = minOf(startY, endY)`, `bottomY = maxOf(startY, endY)` — always correct
- `topX/bottomX` flip via `isReversed` — preserves anchor character on reversal

On reversal past anchor:
- Anchor item transitions from "first" to "last" (or vice versa)
- Its char range recomputes correctly because `bottomX = startX` when reversed
- Items that fall outside new range are removed from captured

### Auto-Scroll During Drag

**Direction-aware**: Only the edge you're dragging toward triggers auto-scroll.
- `endY > startY` → bottom edge only
- `endY < startY` → top edge only
- Opposite edge is inert

Implementation: coroutine loop at ~60fps calling `listState.scrollBy(delta)`.

**Sign with `reverseLayout = true`**: The LazyColumn has `reverseLayout = true`
(ChatView.kt:2204). `scrollBy(positive)` in reversed layout scrolls toward higher
indices (older messages, visually upward). To scroll DOWN visually (toward newer messages),
use `scrollBy(negative)`. So:
- Dragging down → `scrollBy(-speed)` (brings newer messages into view from below)
- Dragging up → `scrollBy(speed)` (brings older messages into view from above)

This is the OPPOSITE of what a non-reversed layout would need. Must verify empirically
as first test — if wrong, flip the sign.

After `scrollBy()`:
- `firstVisibleItemScrollOffset` changes → `snapshotFlow` fires
- `updateSelection(lastPointerY, lastPointerX)` re-evaluates with new item positions
- Selection extends naturally

Auto-scroll does NOT call `updateSelection` directly after `scrollBy` — it relies on
the `snapshotFlow` which fires after layout completes, ensuring `boundsInWindow()` is current.

### Mouse Wheel During Drag

Overlay does NOT consume scroll events. They pass through to LazyColumn.
Same `snapshotFlow { firstVisibleItemScrollOffset }` fires → `updateSelection(lastPointerY, lastPointerX)`.

### Item Loading (Verified — Just Works)

`PreloadItems` (ChatView.kt:2576) reacts to:
```kotlin
snapshotFlow { listState.value.firstVisibleItemIndex }
```
Both `scrollBy()` and mouse wheel change this. Loading pipeline fires automatically.
New items compose → register → `captureIfInRange()`. No special handling needed.

### Eviction Prevention

Our custom trimming in `ChatItemsLoader.kt` disabled during selection:
`allowedTrimming = !selectionActive` (line 186).

LazyColumn's own virtualization still disposes far-off items, but their text
is already captured in the map.

### Platform Gate

All selection code gated on `appPlatform.isDesktop`.

### Swipe-to-Reply on Desktop

`SwipeToDismissModifier` (ChatView.kt:1896) is currently applied unconditionally.
Gate on platform: `if (appPlatform.isDesktop) Modifier else swipeableModifier`.
This is an explicit change, not a side effect of overlay consuming drags.

### RTL Text

The design never assumes text direction. `getOffsetForPosition()` and `getPathForRange()`
are bidi-aware. MarkdownText already switches `LocalLayoutDirection` (TextItemView.kt:82-97).
Must be tested with actual RTL text as part of initial testing, not deferred.

### API Verification (Done)

- `TextLayoutResult.getPathForRange(start, end)` — returns `Path` for highlight
- `PointerEventPass.Initial` — observe without consuming, events flow to children
- `LayoutCoordinates.boundsInWindow()` — absolute window coordinates
- `Modifier.drawBehind` — executes after layout, before content draw
- Material `Text()` does NOT have `onTextLayout` — must use `BasicText()` wrapper
- `contextMenuOpenDetector` (right-click) — independent modifier, non-consuming,
  won't conflict with overlay
- Scroll wheel events are separate from pointer events
- Desktop `EmojiText` is plain `Text()` (EmojiItemView.desktop.kt:20) — can use `BasicText`

---

## Files

### NEW: `TextSelection.kt`

`common/src/commonMain/kotlin/chat/simplex/common/views/chat/TextSelection.kt`

```kotlin
package chat.simplex.common.views.chat

import androidx.compose.runtime.*
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.geometry.Rect
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.TextLayoutResult

val SelectionHighlightColor = Color(0x4D0066FF)

@Stable
data class SelectionCoords(
    val startY: Float,
    val startX: Float,
    val endY: Float,
    val endX: Float
) {
    val isReversed: Boolean get() = startY > endY
    val topY: Float get() = minOf(startY, endY)
    val bottomY: Float get() = maxOf(startY, endY)
    val topX: Float get() = if (isReversed) endX else startX
    val bottomX: Float get() = if (isReversed) startX else endX
}

data class CapturedText(
    val itemId: Long,
    val yPosition: Float,
    val highlightRange: IntRange,
    val text: String
)

interface SelectionParticipant {
    val itemId: Long
    fun getYBounds(): ClosedFloatingPointRange<Float>?
    fun getTextLayoutResult(): TextLayoutResult?
    fun getSelectableEnd(): Int
    fun getAnnotatedText(): String
    fun calculateHighlightRange(coords: SelectionCoords): IntRange?
}

class SelectionManager {
    var coords by mutableStateOf<SelectionCoords?>(null)
        private set

    var isSelecting by mutableStateOf(false)
        private set

    val selectionActive: Boolean get() = coords != null

    var lastPointerWindowY: Float = 0f
        private set
    var lastPointerWindowX: Float = 0f
        private set

    private val participants = mutableListOf<SelectionParticipant>()
    val captured = mutableStateMapOf<Long, CapturedText>()

    fun register(participant: SelectionParticipant) {
        participants.add(participant)
        coords?.let { recomputeParticipant(participant, it) }
    }

    fun unregister(participant: SelectionParticipant) {
        participants.remove(participant)
    }

    fun startSelection(startY: Float, startX: Float) {
        coords = SelectionCoords(startY, startX, startY, startX)
        isSelecting = true
        lastPointerWindowY = startY
        lastPointerWindowX = startX
        captured.clear()
    }

    fun updateSelection(endY: Float, endX: Float) {
        val current = coords ?: return
        coords = current.copy(endY = endY, endX = endX)
        lastPointerWindowY = endY
        lastPointerWindowX = endX
        recomputeAll()
    }

    fun endSelection() {
        isSelecting = false
    }

    fun clearSelection() {
        coords = null
        isSelecting = false
        captured.clear()
    }

    private fun recomputeAll() {
        val c = coords ?: return

        // First pass: identify all visible participants and whether they're in range
        val visibleInRange = mutableMapOf<Long, SelectionParticipant>()
        val visibleOutOfRange = mutableSetOf<Long>()

        for (p in participants) {
            val bounds = p.getYBounds()
            if (bounds != null && bounds.start <= c.bottomY && bounds.endInclusive >= c.topY) {
                visibleInRange[p.itemId] = p
            } else {
                visibleOutOfRange.add(p.itemId)
            }
        }

        // Remove captured entries for visible participants that are now out of range
        visibleOutOfRange.forEach { captured.remove(it) }

        // Update/add captured entries for visible participants in range
        for ((_, p) in visibleInRange) {
            recomputeParticipant(p, c)
        }
    }

    private fun recomputeParticipant(participant: SelectionParticipant, coords: SelectionCoords) {
        val bounds = participant.getYBounds() ?: return
        val highlightRange = participant.calculateHighlightRange(coords) ?: return
        val selectableEnd = participant.getSelectableEnd()
        val clampedStart = highlightRange.first.coerceIn(0, selectableEnd)
        val clampedEnd = highlightRange.last.coerceIn(0, selectableEnd)
        if (clampedStart >= clampedEnd) return

        val annotatedText = participant.getAnnotatedText()
        val text = if (clampedEnd <= annotatedText.length) {
            annotatedText.substring(clampedStart, clampedEnd)
        } else {
            annotatedText.substring(clampedStart.coerceAtMost(annotatedText.length))
        }

        captured[participant.itemId] = CapturedText(
            itemId = participant.itemId,
            yPosition = bounds.start,
            highlightRange = clampedStart until clampedEnd,
            text = text
        )
    }

    fun getSelectedText(): String {
        return captured.values
            .sortedBy { it.yPosition }
            .joinToString("\n") { it.text }
    }

    fun getHighlightRange(itemId: Long): IntRange? {
        return captured[itemId]?.highlightRange
    }
}

fun calculateRangeForElement(
    bounds: Rect?,
    layout: TextLayoutResult?,
    selectableEnd: Int,
    coords: SelectionCoords
): IntRange? {
    bounds ?: return null
    layout ?: return null
    if (selectableEnd <= 0) return null

    val isFirst = bounds.top <= coords.topY && bounds.bottom > coords.topY
    val isLast = bounds.top < coords.bottomY && bounds.bottom >= coords.bottomY
    val isMiddle = bounds.top > coords.topY && bounds.bottom < coords.bottomY

    return when {
        isMiddle -> 0 until selectableEnd
        isFirst && isLast -> {
            val s = layout.getOffsetForPosition(Offset(coords.topX - bounds.left, coords.topY - bounds.top))
            val e = layout.getOffsetForPosition(Offset(coords.bottomX - bounds.left, coords.bottomY - bounds.top))
            minOf(s, e) until maxOf(s, e)
        }
        isFirst -> {
            val s = layout.getOffsetForPosition(Offset(coords.topX - bounds.left, coords.topY - bounds.top))
            s until selectableEnd
        }
        isLast -> {
            val e = layout.getOffsetForPosition(Offset(coords.bottomX - bounds.left, coords.bottomY - bounds.top))
            0 until e
        }
        else -> null
    }
}

val LocalSelectionManager = staticCompositionLocalOf<SelectionManager?> { null }
```

---

### NEW: `SelectionOverlay.kt`

`common/src/desktopMain/kotlin/chat/simplex/common/views/chat/SelectionOverlay.kt`

```kotlin
package chat.simplex.common.views.chat

import androidx.compose.foundation.gestures.awaitEachGesture
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyListState
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.input.pointer.*
import androidx.compose.ui.layout.boundsInWindow
import androidx.compose.ui.layout.onGloballyPositioned
import androidx.compose.ui.layout.positionInWindow
import androidx.compose.ui.platform.LocalViewConfiguration
import kotlinx.coroutines.*

private const val AUTO_SCROLL_ZONE_PX = 40f
private const val MIN_SCROLL_SPEED = 2f
private const val MAX_SCROLL_SPEED = 20f

@Composable
fun SelectionOverlay(
    selectionManager: SelectionManager,
    listState: State<LazyListState>,
    modifier: Modifier = Modifier
) {
    val touchSlop = LocalViewConfiguration.current.touchSlop
    var positionInWindow by remember { mutableStateOf(Offset.Zero) }
    var viewportTop by remember { mutableStateOf(0f) }
    var viewportBottom by remember { mutableStateOf(0f) }
    val scope = rememberCoroutineScope()
    var autoScrollJob by remember { mutableStateOf<Job?>(null) }

    // Re-evaluate selection on scroll (handles mouse wheel and auto-scroll)
    LaunchedEffect(selectionManager) {
        snapshotFlow { listState.value.firstVisibleItemScrollOffset }
            .collect {
                if (selectionManager.isSelecting) {
                    selectionManager.updateSelection(
                        selectionManager.lastPointerWindowY,
                        selectionManager.lastPointerWindowX
                    )
                }
            }
    }

    Box(
        modifier = modifier
            .fillMaxSize()
            .onGloballyPositioned {
                positionInWindow = it.positionInWindow()
                val bounds = it.boundsInWindow()
                viewportTop = bounds.top
                viewportBottom = bounds.bottom
            }
            .pointerInput(selectionManager) {
                awaitEachGesture {
                    val down = awaitPointerEvent(PointerEventPass.Initial)
                    val firstChange = down.changes.first()
                    val localStart = firstChange.position
                    val windowStart = localStart + positionInWindow
                    var totalDrag = Offset.Zero
                    var isDragging = false

                    while (true) {
                        val event = awaitPointerEvent(PointerEventPass.Initial)
                        val change = event.changes.first()

                        if (!change.pressed) {
                            autoScrollJob?.cancel()
                            autoScrollJob = null
                            if (isDragging) {
                                selectionManager.endSelection()
                            }
                            // Non-drag pointer up: do nothing to selection.
                            // Selection persists. Links/right-click work via pass-through.
                            // New drag clears old selection in startSelection().
                            break
                        }

                        totalDrag += change.positionChange()

                        if (!isDragging && totalDrag.getDistance() > touchSlop) {
                            isDragging = true
                            selectionManager.startSelection(windowStart.y, windowStart.x)
                            change.consume()
                        }

                        if (isDragging) {
                            val windowPos = change.position + positionInWindow
                            selectionManager.updateSelection(windowPos.y, windowPos.x)
                            change.consume()

                            // Auto-scroll: direction-aware
                            val draggingDown = windowPos.y > windowStart.y
                            val edgeDistance = if (draggingDown) {
                                viewportBottom - windowPos.y
                            } else {
                                windowPos.y - viewportTop
                            }
                            val shouldAutoScroll = edgeDistance in 0f..AUTO_SCROLL_ZONE_PX

                            if (shouldAutoScroll && autoScrollJob?.isActive != true) {
                                autoScrollJob = scope.launch {
                                    while (isActive && selectionManager.isSelecting) {
                                        val curEdge = if (draggingDown) {
                                            viewportBottom - selectionManager.lastPointerWindowY
                                        } else {
                                            selectionManager.lastPointerWindowY - viewportTop
                                        }
                                        if (curEdge >= AUTO_SCROLL_ZONE_PX) break

                                        val speed = lerp(
                                            MIN_SCROLL_SPEED, MAX_SCROLL_SPEED,
                                            1f - (curEdge / AUTO_SCROLL_ZONE_PX).coerceIn(0f, 1f)
                                        )
                                        // reverseLayout = true:
                                        // drag down (toward newer) = scrollBy(-speed)
                                        // drag up (toward older) = scrollBy(speed)
                                        // VERIFY EMPIRICALLY — if wrong, flip sign
                                        listState.value.scrollBy(if (draggingDown) -speed else speed)
                                        delay(16)
                                    }
                                }
                            } else if (!shouldAutoScroll) {
                                autoScrollJob?.cancel()
                                autoScrollJob = null
                            }
                        }
                    }
                }
            }
    )
}

private fun lerp(start: Float, stop: Float, fraction: Float): Float =
    start + (stop - start) * fraction
```

---

### MODIFY: `TextItemView.kt`

#### MarkdownText (line 59): add parameters

```kotlin
fun MarkdownText(
    // ... existing params ...
    prefix: AnnotatedString? = null,
    selectableEnd: MutableIntState? = null,            // NEW
    annotatedTextState: MutableState<String>? = null,  // NEW
    selectionRange: IntRange? = null,                  // NEW
    onTextLayoutResult: ((TextLayoutResult) -> Unit)? = null  // NEW
)
```

#### Inside `buildAnnotatedString` (BOTH paths — lines 129-139 and 145-257):

After all content appended, before typing indicator and reserve:

```kotlin
// ... all formatted text / plain text content done ...

selectableEnd?.intValue = this.length  // NEW — must be INSIDE builder, before reserve

if (meta?.isLive == true) {
    append(typingIndicator(meta.recent, typingIdx))
}
if (meta != null) withStyle(reserveTimestampStyle) { append(reserve) }
```

This goes in BOTH the `formattedText == null` path (line ~137) and the
`formattedText != null` path (line ~253).

AFTER each `buildAnnotatedString` returns (OUTSIDE the builder), set annotatedTextState.
This must be done in BOTH the `formattedText == null` path AND `formattedText != null` path:
```kotlin
val annotatedText = buildAnnotatedString { ... }
annotatedTextState?.value = annotatedText.text  // NEW — OUTSIDE builder, uses .text API
```

#### ClickableText (line 313): add `selectionRange`

```kotlin
fun ClickableText(
    text: AnnotatedString,
    modifier: Modifier = Modifier,
    style: TextStyle = TextStyle.Default,
    selectionRange: IntRange? = null,  // NEW
    // ... rest unchanged
```

Add highlight before BasicText (line 357):
```kotlin
val selectionHighlight = if (selectionRange != null) {
    Modifier.drawBehind {
        layoutResult.value?.let { result ->
            if (selectionRange.first < selectionRange.last && selectionRange.last <= text.length) {
                drawPath(result.getPathForRange(selectionRange.first, selectionRange.last), SelectionHighlightColor)
            }
        }
    }
} else Modifier

BasicText(
    text = text,
    modifier = modifier.then(selectionHighlight).then(pressIndicator),
    // ... rest unchanged
)
```

#### Three render paths:

**Line 140** (plain text, `formattedText == null`):
- When `meta?.isLive == true`: keep `Text()` (needs `inlineContent` for typing indicator).
  Live messages are excluded from selection anyway.
- Otherwise: replace with `SelectableText()` (see below).

**Line 260** (formatted with links): add `selectionRange` to `ClickableText()` call,
propagate `onTextLayoutResult`:
```kotlin
ClickableText(annotatedText, style = style,
    modifier = modifier.pointerHoverIcon(icon.value),
    selectionRange = selectionRange,
    // ... existing params ...
    onTextLayout = { /* existing */ onTextLayoutResult?.invoke(it) },
    // ...
)
```

**Line 306** (formatted, no links): replace with `SelectableText()`.

#### NEW: `SelectableText` wrapper

Replaces plain `Text()` at lines 140 and 306. Used exactly at these 2 sites.

```kotlin
@Composable
private fun SelectableText(
    text: AnnotatedString,
    style: TextStyle,
    modifier: Modifier = Modifier,
    maxLines: Int = Int.MAX_VALUE,
    overflow: TextOverflow = TextOverflow.Clip,
    selectionRange: IntRange? = null,
    onTextLayoutResult: ((TextLayoutResult) -> Unit)? = null
) {
    val layoutResult = remember { mutableStateOf<TextLayoutResult?>(null) }
    val highlight = if (selectionRange != null) {
        Modifier.drawBehind {
            layoutResult.value?.let { result ->
                if (selectionRange.first < selectionRange.last && selectionRange.last <= text.length) {
                    drawPath(result.getPathForRange(selectionRange.first, selectionRange.last), SelectionHighlightColor)
                }
            }
        }
    } else Modifier

    BasicText(
        text = text,
        modifier = modifier.then(highlight),
        style = style,
        maxLines = maxLines,
        overflow = overflow,
        onTextLayout = {
            layoutResult.value = it
            onTextLayoutResult?.invoke(it)
        }
    )
}
```

**Note**: `BasicText` does not support `inlineContent`. This is why the live message
path (line 140 with `meta?.isLive == true`) keeps `Text()`. Live messages are excluded
from selection (`ci.meta.isLive != true` check in participant registration).

---

### MODIFY: `FramedItemView.kt` — CIMarkdownText participant

Lines 359-385. Full replacement:

```kotlin
@Composable
fun CIMarkdownText(
    chatsCtx: ChatModel.ChatsContext,
    ci: ChatItem,
    chat: Chat,
    chatTTL: Int?,
    linkMode: SimplexLinkMode,
    uriHandler: UriHandler?,
    onLinkLongClick: (link: String) -> Unit = {},
    showViaProxy: Boolean,
    showTimestamp: Boolean,
    prefix: AnnotatedString? = null
) {
    val selectionManager = LocalSelectionManager.current
    val boundsState = remember { mutableStateOf<Rect?>(null) }
    val layoutResultState = remember { mutableStateOf<TextLayoutResult?>(null) }
    val selectableEnd = remember { mutableIntStateOf(Int.MAX_VALUE) }
    val annotatedTextState = remember { mutableStateOf("") }
    val chatInfo = chat.chatInfo
    val text = if (ci.meta.isLive) ci.content.msgContent?.text ?: ci.text else ci.text

    // Register participant (desktop only, not live messages)
    if (selectionManager != null && ci.meta.isLive != true) {
        val currentText = rememberUpdatedState(text)
        val participant = remember(ci.id) {
            object : SelectionParticipant {
                override val itemId = ci.id
                override fun getYBounds() = boundsState.value?.let { it.top..it.bottom }
                override fun getTextLayoutResult() = layoutResultState.value
                override fun getSelectableEnd() = selectableEnd.intValue
                override fun getAnnotatedText(): String {
                    val at = annotatedTextState.value
                    return if (at.isNotEmpty()) at else currentText.value
                }
                override fun calculateHighlightRange(coords: SelectionCoords) =
                    calculateRangeForElement(
                        boundsState.value, layoutResultState.value,
                        selectableEnd.intValue, coords
                    )
            }
        }
        DisposableEffect(participant) {
            selectionManager.register(participant)
            onDispose { selectionManager.unregister(participant) }
        }
    }

    val highlightRange = selectionManager?.getHighlightRange(ci.id)

    Box(
        Modifier
            .padding(vertical = 7.dp, horizontal = 12.dp)
            .onGloballyPositioned { boundsState.value = it.boundsInWindow() }
    ) {
        MarkdownText(
            text, if (text.isEmpty()) emptyList() else ci.formattedText, toggleSecrets = true,
            sendCommandMsg = if (chatInfo.useCommands && chat.chatInfo.sndReady) {
                { msg -> sendCommandMsg(chatsCtx, chat, msg) }
            } else null,
            meta = ci.meta, chatTTL = chatTTL, linkMode = linkMode,
            mentions = ci.mentions, userMemberId = when {
                chatInfo is ChatInfo.Group -> chatInfo.groupInfo.membership.memberId
                else -> null
            },
            uriHandler = uriHandler, senderBold = true, onLinkLongClick = onLinkLongClick,
            showViaProxy = showViaProxy, showTimestamp = showTimestamp, prefix = prefix,
            selectableEnd = selectableEnd,
            annotatedTextState = annotatedTextState,
            selectionRange = highlightRange,
            onTextLayoutResult = { layoutResultState.value = it }
        )
    }
}
```

---

### MODIFY: `EmojiItemView.kt` — participant for emoji messages

The common `EmojiItemView` composable (EmojiItemView.kt:20-28) contains:
```kotlin
Column(...) {
    EmojiText(chatItem.content.text)
    CIMetaView(...)
}
```

Desktop `EmojiText` (EmojiItemView.desktop.kt:15-22) renders:
- Heart emoji: `Image(...)` — skip selection (no text to select)
- All others: `Text(s, style = ...)` — selectable

**Change for desktop** (EmojiItemView.desktop.kt):
Replace `Text()` with `SelectableText()` for non-heart emoji path. Add participant
registration in the common `EmojiItemView` composable:

```kotlin
@Composable
fun EmojiItemView(chatItem: ChatItem, timedMessagesTTL: Int?, showViaProxy: Boolean, showTimestamp: Boolean) {
    val selectionManager = LocalSelectionManager.current
    val boundsState = remember { mutableStateOf<Rect?>(null) }
    val emojiText = chatItem.content.text.trim()
    val currentEmojiText = rememberUpdatedState(emojiText)

    if (selectionManager != null) {
        val participant = remember(chatItem.id) {
            object : SelectionParticipant {
                override val itemId = chatItem.id
                override fun getYBounds() = boundsState.value?.let { it.top..it.bottom }
                override fun getTextLayoutResult() = null // full selection only
                override fun getSelectableEnd() = currentEmojiText.value.length
                override fun getAnnotatedText() = currentEmojiText.value
                override fun calculateHighlightRange(coords: SelectionCoords): IntRange? {
                    val bounds = boundsState.value ?: return null
                    return if (bounds.top <= coords.bottomY && bounds.bottom >= coords.topY)
                        0 until currentEmojiText.value.length
                    else null
                }
            }
        }
        DisposableEffect(participant) {
            selectionManager.register(participant)
            onDispose { selectionManager.unregister(participant) }
        }
    }

    val isSelected = selectionManager?.getHighlightRange(chatItem.id) != null

    Column(
        Modifier
            .padding(vertical = 8.dp, horizontal = 12.dp)
            .onGloballyPositioned { boundsState.value = it.boundsInWindow() },
        horizontalAlignment = Alignment.CenterHorizontally
    ) {
        Box(if (isSelected) Modifier.background(SelectionHighlightColor) else Modifier) {
            EmojiText(chatItem.content.text)
        }
        CIMetaView(chatItem, timedMessagesTTL, showViaProxy = showViaProxy, showTimestamp = showTimestamp)
    }
}
```

Emoji uses `Modifier.background()` for highlight (full selection, no partial character).
`onGloballyPositioned` on the Column captures bounds for Y range check.

---

### MODIFY: `ChatItemsLoader.kt` — eviction prevention

Line 175: add parameter:
```kotlin
private fun removeDuplicatesAndModifySplitsOnBeforePagination(
    // ... existing params ...
    selectionActive: Boolean = false  // NEW
): ModifiedSplits {
```

Line 186: `var allowedTrimming = !selectionActive`

Line 90 (call site): pass `selectionActive = chatState.selectionActive`

Thread `selectionActive` from SelectionManager through ChatState:
add `var selectionActive: Boolean = false` to `ChatModel.ChatsContext.chatState`.
Connected via `LaunchedEffect` in ChatView.kt (see ChatView.kt section above).
SelectionManager writes it in `startSelection` (true) and `clearSelection` (false).

---

### MODIFY: `ChatView.kt`

#### Hoist `listState` to outer scope

Move `listState` creation from inside ChatItemsList (line 1754) to the Box at line 965.
`listState` is currently:
```kotlin
val listState = rememberUpdatedState(rememberSaveable(chatInfo.id, searchValueIsEmpty.value,
    resetListState.value, saver = LazyListState.Saver) { ... })
```
The `chatInfo.id`, `searchValueIsEmpty`, `resetListState` are available at the outer scope.
Pass `listState` down to ChatItemsList as a parameter.

#### Provide SelectionManager (around line 965)

`selectionManager` MUST be defined BEFORE the Box so it's in scope for `onPreviewKeyEvent`:

```kotlin
val selectionManager = if (appPlatform.isDesktop) remember { SelectionManager() } else null
val listState = // ... hoisted from ChatItemsList ...

// Sync selectionActive to ChatState for eviction prevention in ChatItemsLoader
LaunchedEffect(selectionManager) {
    if (selectionManager != null) {
        snapshotFlow { selectionManager.selectionActive }
            .collect { chatsCtx.chatState.selectionActive = it }
    }
}

Box(
    Modifier
        .fillMaxSize()
        .onPreviewKeyEvent { event ->
            if (selectionManager != null && selectionManager.captured.isNotEmpty()
                && event.isCtrlPressed && event.key == Key.C
                && event.type == KeyEventType.KeyDown
            ) {
                clipboard.setText(AnnotatedString(selectionManager.getSelectedText()))
                true
            } else false
        },
    contentAlignment = Alignment.BottomCenter
) {
    CompositionLocalProvider(
        LocalSelectionManager provides selectionManager,
        LocalBringIntoViewSpec provides ...
    ) {
        ChatItemsList(..., listState = listState)

        if (appPlatform.isDesktop && selectionManager != null) {
            SelectionOverlay(selectionManager, listState)
        }
    }
}
```

#### Gate SwipeToDismiss on desktop (line 1896)

```kotlin
val swipeableModifier = if (appPlatform.isDesktop)
    Modifier
else
    SwipeToDismissModifier(state = dismissState, ...)
```

#### Copy button (after FloatingButtons, ~line 2270)

```kotlin
if (appPlatform.isDesktop) {
    val manager = LocalSelectionManager.current
    if (manager != null && manager.captured.isNotEmpty() && !manager.isSelecting) {
        SelectionCopyButton(
            onCopy = {
                clipboard.setText(AnnotatedString(manager.getSelectedText()))
                manager.clearSelection()
            },
            onDismiss = { manager.clearSelection() }
        )
    }
}
```

#### SelectionCopyButton definition (in TextSelection.kt or ChatView.kt)

```kotlin
@Composable
fun SelectionCopyButton(onCopy: () -> Unit, onDismiss: () -> Unit) {
    // Floating pill-shaped button near FloatingButtons area.
    // Dismiss happens when: user starts a new drag (startSelection clears),
    // or user clicks Copy (onCopy calls clearSelection).
    // onDismiss is called by a click-outside scrim or explicit close button if needed.
    Row(
        Modifier
            .padding(8.dp)
            .background(MaterialTheme.colors.surface, RoundedCornerShape(20.dp))
            .border(1.dp, MaterialTheme.colors.onSurface.copy(alpha = 0.12f), RoundedCornerShape(20.dp))
            .clickable { onCopy() }
            .padding(horizontal = 16.dp, vertical = 8.dp),
        verticalAlignment = Alignment.CenterVertically
    ) {
        Icon(painterResource(MR.images.ic_content_copy), null, Modifier.size(16.dp), tint = MaterialTheme.colors.primary)
        Spacer(Modifier.width(6.dp))
        Text(stringResource(MR.strings.copy_verb), color = MaterialTheme.colors.primary)
    }
}
```

---

## Call Chains

### Drag → Highlight

```
Pointer down on overlay
  → awaitPointerEvent(PointerEventPass.Initial) — observe, don't consume
  → pointer moves, totalDrag > touchSlop
    → selectionManager.startSelection(windowY, windowX) — clears any existing selection
    → change.consume() — children stop receiving drag events
    → subsequent moves: selectionManager.updateSelection(windowY, windowX)
      → recomputeAll():
          visible + in range → compute highlightRange, capture text
          visible + out of range → remove from captured
          not visible → keep existing
      → captured (mutableStateMapOf) changes → recomposition
        → CIMarkdownText: manager.getHighlightRange(ci.id) → IntRange
          → MarkdownText → ClickableText/SelectableText
            → drawBehind { getPathForRange(highlightRange) }
```

### Auto-scroll

```
Pointer near viewport edge, dragging toward that edge
  → autoScrollJob coroutine launched
    → loop: listState.scrollBy(delta) at ~60fps
      → firstVisibleItemIndex changes
        → PreloadItems snapshotFlow fires → loads from DB → new items compose → register
      → firstVisibleItemScrollOffset changes
        → snapshotFlow in SelectionOverlay fires
          → updateSelection(lastPointerY, lastPointerX)
            → items have new boundsInWindow → captured map extends/updates
```

### Mouse wheel during drag

```
User holds left button + scrolls wheel
  → scroll event NOT consumed by overlay → reaches LazyColumn → scrolls
  → firstVisibleItemScrollOffset changes → snapshotFlow fires
    → updateSelection(lastPointerY, lastPointerX) → recompute with new positions
```

### Direction reversal

```
Click Y=500 item A char 15, drag to Y=700 item C
  topY=500 bottomY=700 topX=startX bottomX=endX
  captured: {A: hl=15..end, B: hl=0..end, C: hl=0..charAtEndX}

Reverse to Y=400 (above A, into item Z)
  topY=400 bottomY=500 topX=endX bottomX=startX (flipped: isReversed=true)
  Z: "first" → hl=charAtTopX..selectableEnd
  A: "last" → hl=0..charAtBottomX (bottomX=startX → original anchor char 15)
  B, C: out of range → removed from captured
  captured: {Z: hl=charAtTopX..end, A: hl=0..15}
```

### Click on link (no selection interference)

```
Pointer down on overlay
  → awaitPointerEvent(Initial) — observed, NOT consumed
  → pointer up without exceeding touchSlop
    → isDragging = false → no clearSelection, no endSelection
    → event was never consumed → passes through to children
    → combinedClickable on ChatItemView sees full down→up sequence
      → onClick fires → link handler invoked
```

---

## Verify Before First Test

1. **Auto-scroll sign**: Run with `scrollBy(-speed)` for dragging down. If content
   scrolls wrong direction, flip to `scrollBy(speed)`. One-line change.

2. **`onPreviewKeyEvent` receives Ctrl+C**: The outer Box must be in the key event
   propagation path. Verify by logging.

3. **`boundsInWindow()` consistency**: Log overlay pointer window coords and item
   boundsInWindow for the same physical position. Must match.

---

## Deferred (Explicit)

- Quote content selection
- Date separator selection
- Sender name / timestamp selection
- Chat event text selection ("Alice joined the group")
- Copy button positioning relative to visible selected items after scroll

---

## Testing (All v1)

1. Single message partial character selection
2. Multi-message selection with highlights
3. Highlight stops before invisible reserve space (no highlight on timestamp area)
4. Copy produces clean text (no invisible chars, no timestamps)
5. Ctrl+C copies selected text
6. Copy button appears after drag and works
7. Click on links not consumed by overlay
8. Long click context menu works
9. Right-click doesn't affect selection
10. Scroll wheel during active drag extends selection
11. Auto-scroll bottom edge (downward drag only)
12. Auto-scroll top edge (upward drag only)
13. No auto-scroll at opposite edge
14. Auto-scroll triggers item loading from DB
15. Direction reversal past anchor preserves anchor character
16. Selection shrinks on reverse (items removed from captured)
17. Selection persists after drag end and across scroll
18. New drag clears previous selection
19. Emoji-only message fully selectable
20. Live messages excluded from selection
21. Edited messages reflect current text (rememberUpdatedState)
22. Swipe-to-reply disabled on desktop
23. RTL text selection and highlight
24. Multi-line message selection
25. Mentions with different display length selected correctly
