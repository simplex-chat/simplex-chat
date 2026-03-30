# Desktop Text Selection Plan

## Goal
Implement cross-message text selection on desktop (Compose Multiplatform) that:
1. Works like browser selection (click+drag, with auto-scroll)
2. Copies text with timestamps and sender names
3. Shows visual highlight on all selected elements (messages, names, timestamps, dates)
4. Shows copy button on mouse release
5. Selection persists across scroll

## Architecture Overview

### Core Idea
- **Overlay** on top of LazyColumnWithScrollBar handles drag detection
- Overlay uses `PointerEventPass.Initial` to observe without consuming
- Only consumes pointer move/drag events after drag threshold exceeded
- Scroll wheel events are **never consumed** — they pass through to LazyColumn
- Selection state is a **continuously-resolved map** of `(itemId, elementType) → CapturedText`
  with `charRange`, recomputed on every pointer move and scroll event
- Window coordinates are ephemeral input for computing the resolved map; the map is
  the real selection state and is what rendering reads
- Text elements draw their own highlights using `TextLayoutResult.getPathForRange()`
- **Item eviction disabled** during active selection (our trimming; LazyColumn's own
  virtualization is handled by eager capture)
- **Auto-scroll** when pointer near viewport edge during drag, direction-aware

### Selection State Model

No two-phase "dragging vs resolved" distinction. Selection is always a resolved map:

```kotlin
data class CapturedText(
    val itemId: Long,
    val elementType: SelectableElementType,
    val yPosition: Float,
    val text: String,
    val charRange: IntRange
)

// The selection state — always item-based, always current:
val captured: MutableMap<Pair<Long, SelectableElementType>, CapturedText>
```

On every `updateSelection()` call (triggered by pointer move OR scroll change):
1. Recompute which visible participants are in range from current coordinates
2. **Remove** entries for items no longer in range (selection shrunk)
3. **Update** char ranges for items still in range (endpoint moved)
4. **Add** entries for newly-in-range items (selection grew)
5. Items that scrolled out of view: their entry stays (participant gone, can't recompute,
   but captured text is still valid)

Rendering reads `captured[myId to myType]?.charRange` — no coordinate comparison during render.

### Coordinate System (VERIFIED)

All coordinates are in **window space** during drag computation:
- Overlay tracks its position via `onGloballyPositioned { positionInWindow = it.positionInWindow() }`
- Overlay transforms pointer events: `windowPos = positionInWindow + localPointerPos`
- Items report bounds via `onGloballyPositioned { bounds = it.boundsInWindow() }`
- `calculateRangeForElement` adjusts X by `bounds.left` for `getOffsetForPosition`

Window coordinates are consistent within a single frame. After scroll, items have new
`boundsInWindow()` values, so re-calling `updateSelection()` with the same pointer position
produces correct results against the new item positions.

### Bidirectional Selection (Core Design, Not Edge Case)

Users drag in any direction, change direction mid-drag, and reverse past the anchor point.

`SelectionCoords`:
- `startY/startX` = anchor (where click happened, never changes during drag)
- `endY/endX` = current pointer position (changes on every move)
- `topY = minOf(startY, endY)`, `bottomY = maxOf(startY, endY)` — always correct
- `topX/bottomX` flip based on `isReversed` — preserves anchor character

When user reverses past anchor:
- Anchor item transitions from "first" to "last" (or vice versa)
- Its char range recomputes correctly because `bottomX = startX` when reversed
- Items that were selected get removed from captured map when they fall outside
  the new `topY..bottomY` range

### Auto-Scroll During Drag

When pointer is near the viewport edge **in the direction of drag**, auto-scroll the list.

**Direction-aware**: Only the edge you're dragging toward triggers auto-scroll.
- `endY > startY` (dragging down) → only bottom edge auto-scrolls
- `endY < startY` (dragging up) → only top edge auto-scrolls
- The opposite edge is inert — prevents unwanted scroll when selecting a few items
  near the top/bottom of the viewport

**Implementation**: A coroutine loop during drag:
```kotlin
// Inside SelectionOverlay, during active drag
while (isDragging) {
    val edgeDistance = if (draggingDown) {
        viewportBottom - lastPointerY  // distance to bottom edge
    } else {
        lastPointerY - viewportTop     // distance to top edge
    }

    if (edgeDistance < AUTO_SCROLL_ZONE) {
        val speed = lerp(MIN_SCROLL_SPEED, MAX_SCROLL_SPEED, 1f - edgeDistance / AUTO_SCROLL_ZONE)
        listState.scrollBy(if (draggingDown) speed else -speed)
        // After scroll, items moved — re-evaluate selection with same pointer position
        selectionManager.updateSelection(lastPointerWindowY, lastPointerWindowX)
    }
    delay(16) // ~60fps
}
```

After `listState.scrollBy()`:
- Items recompose with new `boundsInWindow()` positions
- `updateSelection()` with same pointer coords intersects different items now
- Selection naturally extends as new items scroll into view
- Existing captured entries for scrolled-out items are preserved

### Mouse Wheel During Drag

User holds mouse button and scrolls wheel. Pointer stays still, content moves.

The overlay does NOT consume scroll wheel events (only pointer move/drag events via
`change.consume()`). Scroll events pass through to LazyColumn.

After wheel scroll, we need to re-evaluate selection. A `snapshotFlow` on scroll state
triggers `updateSelection()` with the last known pointer position:

```kotlin
// In the composable hosting the overlay
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
```

This ensures the captured map stays current even when the user scrolls without moving the pointer.

### Item Loading During Scroll (VERIFIED — Just Works)

Item loading (pagination) is triggered reactively via:
```kotlin
// ChatView.kt line 2576
snapshotFlow { listState.value.firstVisibleItemIndex }
    .distinctUntilChanged()
    .collect { firstVisibleIndex ->
        preloadItemsBefore(...)
        preloadItemsAfter(...)
        loadLastItems(...)
    }
```

This reacts to `firstVisibleItemIndex` changes, not to scroll events directly.
Both auto-scroll (`listState.scrollBy()`) and mouse wheel scroll change
`firstVisibleItemIndex`, so the existing loading pipeline fires automatically.

New items loaded → composed → register as participants → `register()` calls
`captureIfInRange()` → captured if within current selection. No special handling needed.

### Platform Gate
All selection code is gated on `appPlatform.isDesktop`. Mobile uses native selection.

### API Verification (DONE)
- `TextLayoutResult.getPathForRange(start, end)` — confirmed, returns `Path` for drawing
- `PointerEventPass.Initial` — confirmed: observe without consuming, events flow to children
- `LayoutCoordinates.boundsInWindow()` / `positionInWindow()` — confirmed, absolute window coords
- `Modifier.drawBehind` — confirmed: executes after layout, before content draw
- Scroll wheel events are separate from pointer move events in Compose — overlay can consume
  one without affecting the other

---

## File Changes

### 1. NEW FILE: `common/src/commonMain/kotlin/chat/simplex/common/views/chat/TextSelection.kt`

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
    val elementType: SelectableElementType,
    val yPosition: Float,
    val text: String,
    val charRange: IntRange
)

enum class SelectableElementType {
    MESSAGE_CONTENT,
    SENDER_NAME,
    TIMESTAMP,
    QUOTE_CONTENT,
    QUOTE_SENDER,
    DATE_SEPARATOR
}

interface SelectionParticipant {
    val itemId: Long
    val elementType: SelectableElementType
    fun getYBounds(): ClosedFloatingPointRange<Float>?
    fun getTextLayoutResult(): TextLayoutResult?
    fun getText(): String
    fun calculateSelectionRange(coords: SelectionCoords): IntRange?
}

class SelectionManager {
    var coords by mutableStateOf<SelectionCoords?>(null)
        private set

    var isSelecting by mutableStateOf(false)
        private set

    /** Used by ChatItemsLoader to disable trimming. */
    val selectionActive: Boolean get() = coords != null

    /** Last pointer position in window coords — used for re-evaluation on scroll. */
    var lastPointerWindowY: Float = 0f
        private set
    var lastPointerWindowX: Float = 0f
        private set

    private val participants = mutableListOf<SelectionParticipant>()

    /** The resolved selection state. Always current, always item-based. */
    val captured = mutableStateMapOf<Pair<Long, SelectableElementType>, CapturedText>()

    fun register(participant: SelectionParticipant) {
        participants.add(participant)
        coords?.let { recomputeParticipant(participant, it) }
    }

    fun unregister(participant: SelectionParticipant) {
        participants.remove(participant)
        // Do NOT remove from captured — text already captured survives disposal
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
        // coords and captured remain for highlight display + copy
    }

    fun clearSelection() {
        coords = null
        isSelecting = false
        captured.clear()
    }

    /**
     * Recompute captured map from all visible participants.
     * - Visible and in range: add/update
     * - Visible and NOT in range: remove
     * - Not visible (unregistered): keep existing capture
     */
    private fun recomputeAll() {
        val c = coords ?: return
        val visibleIds = mutableSetOf<Pair<Long, SelectableElementType>>()

        for (p in participants) {
            val key = p.itemId to p.elementType
            visibleIds.add(key)
            recomputeParticipant(p, c)
        }

        // Remove entries for visible participants that are no longer in selection
        // (but keep entries for non-visible participants — they scrolled out)
        captured.keys.removeAll { key ->
            key in visibleIds && key !in captured
        }
        // The above has a subtle issue — let's be explicit:
        val toRemove = mutableListOf<Pair<Long, SelectableElementType>>()
        for (key in captured.keys) {
            if (key in visibleIds) {
                // Participant is visible — check if still in range
                val p = participants.find { it.itemId == key.first && it.elementType == key.second }
                if (p != null) {
                    val bounds = p.getYBounds()
                    if (bounds == null || bounds.start > c.bottomY || bounds.endInclusive < c.topY) {
                        toRemove.add(key)
                    }
                }
            }
            // If key NOT in visibleIds → participant scrolled out → keep capture
        }
        toRemove.forEach { captured.remove(it) }
    }

    private fun recomputeParticipant(participant: SelectionParticipant, coords: SelectionCoords) {
        val bounds = participant.getYBounds() ?: return
        val key = participant.itemId to participant.elementType

        if (bounds.start > coords.bottomY || bounds.endInclusive < coords.topY) {
            // Out of range — will be cleaned up by recomputeAll if visible
            return
        }

        val range = participant.calculateSelectionRange(coords) ?: return
        val fullText = participant.getText()
        val clampedStart = range.first.coerceIn(0, fullText.length)
        val clampedEnd = range.last.coerceIn(0, fullText.length)
        if (clampedStart >= clampedEnd) return

        captured[key] = CapturedText(
            itemId = participant.itemId,
            elementType = participant.elementType,
            yPosition = bounds.start,
            text = fullText.substring(clampedStart, clampedEnd),
            charRange = clampedStart until clampedEnd
        )
    }

    fun getSelectedText(): String {
        return captured.values
            .sortedBy { it.yPosition }
            .groupBy { it.itemId }
            .values
            .joinToString("\n") { elements ->
                val date = elements.find { it.elementType == SelectableElementType.DATE_SEPARATOR }?.text
                val timestamp = elements.find { it.elementType == SelectableElementType.TIMESTAMP }?.text ?: ""
                val sender = elements.find { it.elementType == SelectableElementType.SENDER_NAME }?.text ?: ""
                val content = elements.find { it.elementType == SelectableElementType.MESSAGE_CONTENT }?.text ?: ""

                if (date != null) {
                    date
                } else {
                    buildString {
                        if (timestamp.isNotEmpty()) append("[$timestamp] ")
                        if (sender.isNotEmpty()) append("$sender: ")
                        append(content)
                    }.trim()
                }
            }
    }

    /**
     * Query selection for rendering. Returns charRange from captured map if present.
     * No coordinate comparison — uses the resolved map directly.
     */
    fun getSelectionRange(itemId: Long, elementType: SelectableElementType): IntRange? {
        return captured[itemId to elementType]?.charRange
    }
}

/**
 * Shared helper: compute char range for a text element given its bounds, layout, and selection coords.
 * Used by all participant types that have TextLayoutResult.
 */
fun calculateRangeForElement(
    bounds: Rect?,
    layout: TextLayoutResult?,
    text: String,
    coords: SelectionCoords
): IntRange? {
    bounds ?: return null
    layout ?: return null
    if (text.isEmpty()) return null

    val isFirst = bounds.top <= coords.topY && bounds.bottom > coords.topY
    val isLast = bounds.top < coords.bottomY && bounds.bottom >= coords.bottomY
    val isMiddle = bounds.top > coords.topY && bounds.bottom < coords.bottomY

    return when {
        isMiddle -> 0 until text.length
        isFirst && isLast -> {
            val s = layout.getOffsetForPosition(Offset(coords.topX - bounds.left, coords.topY - bounds.top))
            val e = layout.getOffsetForPosition(Offset(coords.bottomX - bounds.left, coords.bottomY - bounds.top))
            minOf(s, e) until maxOf(s, e)
        }
        isFirst -> {
            val s = layout.getOffsetForPosition(Offset(coords.topX - bounds.left, coords.topY - bounds.top))
            s until text.length
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

### 2. NEW FILE: `common/src/desktopMain/kotlin/chat/simplex/common/views/chat/SelectionOverlay.kt`

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

    // Re-evaluate selection on scroll changes (handles mouse wheel during drag)
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
                    val localStart = down.changes.first().position
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
                            } else {
                                selectionManager.clearSelection()
                            }
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
                            val shouldAutoScroll = edgeDistance < AUTO_SCROLL_ZONE_PX
                                    && edgeDistance >= 0

                            if (shouldAutoScroll && autoScrollJob?.isActive != true) {
                                autoScrollJob = scope.launch {
                                    while (isActive && selectionManager.isSelecting) {
                                        val currentEdge = if (draggingDown) {
                                            viewportBottom - selectionManager.lastPointerWindowY
                                        } else {
                                            selectionManager.lastPointerWindowY - viewportTop
                                        }
                                        if (currentEdge >= AUTO_SCROLL_ZONE_PX) break

                                        val speed = lerp(
                                            MIN_SCROLL_SPEED, MAX_SCROLL_SPEED,
                                            1f - (currentEdge / AUTO_SCROLL_ZONE_PX).coerceIn(0f, 1f)
                                        )
                                        listState.value.scrollBy(
                                            if (draggingDown) speed else -speed
                                        )
                                        // After scroll, items moved — re-evaluate
                                        selectionManager.updateSelection(
                                            selectionManager.lastPointerWindowY,
                                            selectionManager.lastPointerWindowX
                                        )
                                        delay(16) // ~60fps
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

### 3. MODIFY: `ChatItemsLoader.kt` — Disable eviction during selection

**Line 175-186: Add `selectionActive` parameter:**

```kotlin
private fun removeDuplicatesAndModifySplitsOnBeforePagination(
  unreadAfterItemId: StateFlow<Long>,
  newItems: SnapshotStateList<ChatItem>,
  newIds: Set<Long>,
  splits: StateFlow<List<Long>>,
  visibleItemIndexesNonReversed: () -> IntRange,
  selectionActive: Boolean = false  // NEW
): ModifiedSplits {
  // ...
  var allowedTrimming = !selectionActive  // CHANGED: was `true`
```

**Line 90: Pass selectionActive at call site:**

```kotlin
val (oldUnreadSplitIndex, newUnreadSplitIndex, trimmedIds, newSplits) =
    removeDuplicatesAndModifySplitsOnBeforePagination(
        unreadAfterItemId, newItems, newIds, splits, visibleItemIndexesNonReversed,
        selectionActive = chatState.selectionActive  // NEW
    )
```

**Threading**: Add `var selectionActive: Boolean = false` to `ChatModel.ChatsContext.chatState`.
SelectionManager writes it on start/clear.

---

### 4. MODIFY: `TextItemView.kt`

#### 4a. ClickableText — add selectionRange and highlight (line 313-369)

**Add parameter:**
```kotlin
fun ClickableText(
  text: AnnotatedString,
  modifier: Modifier = Modifier,
  style: TextStyle = TextStyle.Default,
  selectionRange: IntRange? = null,  // NEW
  // ... rest unchanged
```

**Add highlight before BasicText (around line 357):**
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

#### 4b. MarkdownText — add parameters (line 59)

```kotlin
fun MarkdownText (
  // ... existing params ...
  prefix: AnnotatedString? = null,
  selectionRange: IntRange? = null,             // NEW
  onTextLayoutResult: ((TextLayoutResult) -> Unit)? = null  // NEW
)
```

#### 4c. Handle all three render paths

**Line 140** — plain text, no formatting: replace `Text()` with `SelectableText()` (see 4d).

**Line 260** — formatted with links: add `selectionRange` to `ClickableText()` call,
propagate `onTextLayoutResult`.

**Line 306** — formatted, no links: replace `Text()` with `SelectableText()`.

#### 4d. NEW: SelectableText wrapper

```kotlin
@Composable
private fun SelectableText(
  text: AnnotatedString,
  style: TextStyle,
  modifier: Modifier = Modifier,
  maxLines: Int = Int.MAX_VALUE,
  overflow: TextOverflow = TextOverflow.Clip,
  selectionRange: IntRange? = null,
  onTextLayoutResult: ((TextLayoutResult) -> Unit)? = null,
  inlineContent: Map<String, InlineTextContent> = mapOf()
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

---

### 5. MODIFY: `FramedItemView.kt` — Participant registration for message content

CIMarkdownText (lines 359-385):

```kotlin
@Composable
fun CIMarkdownText(
  // ... existing params ...
) {
  val selectionManager = LocalSelectionManager.current
  val boundsState = remember { mutableStateOf<Rect?>(null) }
  val layoutResultState = remember { mutableStateOf<TextLayoutResult?>(null) }
  val chatInfo = chat.chatInfo
  val text = if (ci.meta.isLive) ci.content.msgContent?.text ?: ci.text else ci.text

  if (selectionManager != null) {
    val participant = remember(ci.id) {
      object : SelectionParticipant {
        override val itemId = ci.id
        override val elementType = SelectableElementType.MESSAGE_CONTENT
        override fun getYBounds() = boundsState.value?.let { it.top..it.bottom }
        override fun getTextLayoutResult() = layoutResultState.value
        override fun getText() = text
        override fun calculateSelectionRange(coords: SelectionCoords) =
            calculateRangeForElement(boundsState.value, layoutResultState.value, getText(), coords)
      }
    }
    DisposableEffect(participant) {
      selectionManager.register(participant)
      onDispose { selectionManager.unregister(participant) }
    }
  }

  // Rendering reads from resolved map — no coordinate comparison
  val selectionRange = selectionManager?.getSelectionRange(ci.id, SelectableElementType.MESSAGE_CONTENT)

  Box(
    Modifier
      .padding(vertical = 7.dp, horizontal = 12.dp)
      .onGloballyPositioned { boundsState.value = it.boundsInWindow() }
  ) {
    MarkdownText(
      text, if (text.isEmpty()) emptyList() else ci.formattedText, toggleSecrets = true,
      // ... existing params ...
      selectionRange = selectionRange,
      onTextLayoutResult = { layoutResultState.value = it }
    )
  }
}
```

---

### 6. MODIFY: `ChatView.kt`

#### 6a. Provide SelectionManager and overlay

Around line 965:
```kotlin
Box(Modifier.fillMaxSize(), contentAlignment = Alignment.BottomCenter) {
  val selectionManager = if (appPlatform.isDesktop) remember { SelectionManager() } else null
  CompositionLocalProvider(
    LocalSelectionManager provides selectionManager,
    LocalBringIntoViewSpec provides object : BringIntoViewSpec {
      override fun calculateScrollDistance(offset: Float, size: Float, containerSize: Float): Float = 0f
    }
  ) {
    ChatItemsList(...)

    // Overlay on top of list, after it in Z-order
    if (appPlatform.isDesktop && selectionManager != null) {
      SelectionOverlay(selectionManager, listState)
    }
  }
```

#### 6b. Sender name participant (MemberNameAndRole, line 1948)

Register participant, render `Modifier.background(SelectionHighlightColor)` when
`selectionManager.getSelectionRange(cItem.id, SENDER_NAME) != null`.
Full selection only (no partial char selection for names).

#### 6c. Timestamp participant (CIMetaView)

Same pattern as sender name. Full selection only.

#### 6d. Date separator participant (DateSeparator)

Same pattern. Use `itemId = -date.epochSeconds` for unique ID.

#### 6e. Copy button (after FloatingButtons, ~line 2270)

```kotlin
if (appPlatform.isDesktop) {
    val manager = LocalSelectionManager.current
    if (manager != null && !manager.captured.isEmpty() && !manager.isSelecting) {
        val clipboard = LocalClipboardManager.current
        SelectionCopyButton(
            coords = manager.coords!!,
            onCopy = {
                clipboard.setText(AnnotatedString(manager.getSelectedText()))
                manager.clearSelection()
            },
            onDismiss = { manager.clearSelection() }
        )
    }
}
```

---

## Call Chain Diagrams

### Drag → Highlight (continuous resolution)

```
Pointer move during drag
       │
       ▼
SelectionOverlay: change.consume(), convert to window coords
       │
       ▼
selectionManager.updateSelection(windowY, windowX)
       │
       ├─ coords updated
       ├─ recomputeAll():
       │    for each visible participant:
       │      in range? → compute charRange → update captured map
       │      out of range? → remove from captured
       │    scrolled-out entries → untouched
       │
       ▼
captured map is mutableStateMapOf → triggers recomposition
       │
       ▼
CIMarkdownText: selectionRange = manager.getSelectionRange(myId, MESSAGE_CONTENT)
       │                          (reads captured map, no coordinate math)
       ▼
MarkdownText → ClickableText/SelectableText
       │
       ▼
drawBehind { getPathForRange(charRange) → drawPath(highlight) }
```

### Auto-scroll

```
Pointer near viewport bottom edge, dragging downward
       │
       ▼
autoScrollJob launched (coroutine loop at ~60fps)
       │
       ▼
listState.scrollBy(speed)   ──────────────────────────────────┐
       │                                                       │
       ▼                                                       ▼
Items move (new boundsInWindow)              firstVisibleItemIndex changes
       │                                                       │
       ▼                                                       ▼
updateSelection(samePointerY, samePointerX)  preloadItemsBefore/After fires
       │                                     (loads new items from DB)
       ▼                                                       │
Intersection with new bounds →                                 ▼
selection extends to new items              New items compose → register
                                            → captureIfInRange → captured
```

### Mouse wheel during drag

```
User holds mouse button + scrolls wheel
       │
       ├─ Scroll event NOT consumed by overlay → reaches LazyColumn → scrolls
       │
       ▼
snapshotFlow { firstVisibleItemScrollOffset } fires
       │
       ▼
selectionManager.updateSelection(lastPointerWindowY, lastPointerWindowX)
       │
       ▼
Items have new boundsInWindow → recomputeAll → captured map updated
```

### Direction reversal past anchor

```
User clicks at Y=500 in item A (char 15), drags to Y=700 (item C)
  → topY=500, bottomY=700
  → item A: first (char 15..end), items B: middle (full), item C: last (0..char)

User reverses to Y=400 (above item A, in item Z)
  → topY=400, bottomY=500
  → item Z: first (char..end)
  → item A: last (0..char 15) — anchor char preserved via bottomX=startX
  → items B, C: out of range → removed from captured
```

---

## Testing Checklist

1. [ ] Single message partial selection highlights correct characters
2. [ ] Multi-message selection highlights all messages in range
3. [ ] Selection highlight follows styled text (bold, italic, links) correctly
4. [ ] Sender names highlight when in selection range
5. [ ] Timestamps highlight when in selection range
6. [ ] Date separators highlight when in selection range
7. [ ] Copy produces formatted text: `[timestamp] sender: message`
8. [ ] Click on links still works (not consumed by overlay)
9. [ ] Long click context menu still works
10. [ ] Right click context menu still works
11. [ ] Scroll wheel during active drag extends selection correctly
12. [ ] Auto-scroll when dragging to bottom edge (downward drag only)
13. [ ] Auto-scroll when dragging to top edge (upward drag only)
14. [ ] Auto-scroll does NOT trigger at opposite edge
15. [ ] Auto-scroll loads additional items from DB as they come into view
16. [ ] Direction reversal: drag down then up past anchor works
17. [ ] Direction reversal: selection shrinks correctly (items removed from captured)
18. [ ] Selection persists after drag ends (highlight stays, copy button shows)
19. [ ] Click elsewhere clears selection
20. [ ] Items scrolling into view during selection get captured
21. [ ] Items eviction (our trimming) disabled during selection
22. [ ] Works with RTL text
23. [ ] Works with multi-line messages
24. [ ] Works with emoji-only messages

---

## Decisions (Resolved)

1. **Timestamps/sender names**: Separately highlighted (WYSIWYG)
2. **Date separators**: Selectable with highlight
3. **Selection + scroll**: Persists, auto-scroll supported, eviction disabled
4. **Selection state model**: Continuously resolved map, no two-phase
5. **Bidirectional drag**: Core design, anchor preserved on reversal
6. **Auto-scroll direction**: Only the edge you're dragging toward
7. **Quote selection**: Defer to later phase

---

## Implementation Order

1. **Phase 1**: TextSelection.kt + SelectionOverlay.kt + highlight on single message
2. **Phase 2**: Multi-message + participant registration in CIMarkdownText
3. **Phase 3**: Copy button + `getSelectedText()`
4. **Phase 4**: Sender name, timestamp, date separator participants
5. **Phase 5**: Eviction prevention in ChatItemsLoader.kt
6. **Phase 6**: Auto-scroll during drag
7. **Phase 7**: Mouse wheel during drag (snapshotFlow re-evaluation)
8. **Phase 8**: Polish (RTL, color tuning, edge cases)
