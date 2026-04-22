package chat.simplex.common.views.chat

import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.clickable
import androidx.compose.foundation.focusable
import androidx.compose.foundation.gestures.awaitEachGesture
import androidx.compose.foundation.gestures.awaitFirstDown
import androidx.compose.foundation.gestures.scrollBy
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyListState
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.draw.clip
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.focus.focusRequester
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.geometry.Rect
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.input.key.*
import androidx.compose.ui.input.pointer.*
import androidx.compose.ui.layout.boundsInWindow
import androidx.compose.ui.layout.onGloballyPositioned
import androidx.compose.ui.layout.onSizeChanged
import androidx.compose.ui.layout.positionInWindow
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.platform.LocalViewConfiguration
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.TextLayoutResult
import androidx.compose.ui.unit.IntOffset
import androidx.compose.ui.unit.IntSize
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.chat.item.itemSegmentDisplayText
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import kotlinx.coroutines.*

val SelectionHighlightColor = Color(0x4D0066FF)

data class ItemContext(
    val selectionIndex: Int = -1
)

val LocalItemContext = compositionLocalOf { ItemContext() }

// Selection is anchored to ChatItem IDs, not list positions, so it survives list mutations
// (new message arrives, item deleted, etc.). Positional indices are derived from the current
// items list on read — see SelectionManager.startIndex / endIndex.
data class SelectionRange(
    val startItemId: Long,
    val startOffset: Int,
    val endItemId: Long,
    val endOffset: Int
)

enum class SelectionState { Idle, Selecting, Selected }

class SelectionManager {
    var selectionState by mutableStateOf(SelectionState.Idle)
        private set

    var range by mutableStateOf<SelectionRange?>(null)
        private set

    // Current merged-items list, kept in sync by SelectionHandler so the manager can resolve
    // item-id-anchored positions without callers threading the list through every call site.
    var items by mutableStateOf<List<MergedItem>>(emptyList())

    var anchorWindowY by mutableStateOf(0f)
        private set
    var anchorWindowX by mutableStateOf(0f)
        private set
    var focusWindowY by mutableStateOf(0f)
    var focusWindowX by mutableStateOf(0f)
    var viewportWidth by mutableStateOf(0f)
    var viewportHeight by mutableStateOf(0f)
    var viewportTop by mutableStateOf(0f)
    var viewportBottom by mutableStateOf(0f)
    var viewportPosition by mutableStateOf(Offset.Zero)
    var focusCharRect by mutableStateOf(Rect.Zero) // X: absolute window, Y: relative to item
    var listState: State<LazyListState>? = null
    var onCopySelection: (() -> Unit)? = null
    private var autoScrollJob: Job? = null

    // Positional indices derived from anchored item IDs against the current list.
    // Cached via derivedStateOf so the O(n) scan runs once per items/range change, not once per
    // per-item composable read (which would be O(n²)). Returns -1 if the anchored item is gone.
    private val startIndexState = derivedStateOf { items.indexOfItemId(range?.startItemId) }
    private val endIndexState = derivedStateOf { items.indexOfItemId(range?.endItemId) }
    val startIndex: Int get() = startIndexState.value
    val endIndex: Int get() = endIndexState.value

    fun startSelection(startIndex: Int, anchorY: Float, anchorX: Float) {
        val id = items.getOrNull(startIndex)?.newest()?.item?.id ?: return
        range = SelectionRange(id, -1, id, -1)
        selectionState = SelectionState.Selecting
        anchorWindowY = anchorY
        anchorWindowX = anchorX
    }

    fun setAnchorOffset(offset: Int) {
        val r = range ?: return
        range = r.copy(startOffset = offset)
    }

    fun updateFocusIndex(index: Int) {
        val r = range ?: return
        val id = items.getOrNull(index)?.newest()?.item?.id ?: return
        range = r.copy(endItemId = id)
    }

    fun updateFocusOffset(offset: Int, charRect: Rect = Rect.Zero) {
        val r = range ?: return
        range = r.copy(endOffset = offset)
        focusCharRect = charRect
    }

    fun endSelection() {
        autoScrollJob?.cancel()
        autoScrollJob = null
        selectionState = SelectionState.Selected
    }

    // Snaps boundary offsets to include full transformed segments (mentions, links with showText).
    fun snapSelection(linkMode: SimplexLinkMode) {
        val r = range ?: return
        val s = startIndex
        val e = endIndex
        if (s < 0 || e < 0) return
        val startCi = items[s].newest().item
        val endCi = items[e].newest().item
        // expandRight: snap in the direction that grows the selection
        val startExpandRight = if (s == e) r.startOffset > r.endOffset else s < e
        val endExpandRight = if (s == e) r.endOffset > r.startOffset else e < s
        val snappedStart = if (r.startOffset >= 0)
            snapOffset(startCi, r.startOffset, linkMode, expandRight = startExpandRight)
        else r.startOffset
        val snappedEnd = if (r.endOffset >= 0)
            snapOffset(endCi, r.endOffset, linkMode, expandRight = endExpandRight)
        else r.endOffset
        if (snappedStart != r.startOffset || snappedEnd != r.endOffset) {
            range = r.copy(startOffset = snappedStart, endOffset = snappedEnd)
        }
    }

    fun clearSelection() {
        range = null
        selectionState = SelectionState.Idle
    }

    // Computes copy button position relative to the viewport (called during layout phase).
    // Dragging down: button below focus char (top-left at char's bottom-right corner).
    // Dragging up: button above focus char (bottom-right at char's top-left corner).
    // focusCharRect X is absolute window coords, Y is relative to item.
    fun copyButtonOffset(draggingDown: Boolean, gap: Float, buttonSize: IntSize): IntOffset {
        val e = endIndex
        if (e < 0) return IntOffset.Zero
        val ls = listState?.value ?: return IntOffset.Zero
        val itemInfo = ls.layoutInfo.visibleItemsInfo.find { it.index == e }
            ?: return IntOffset(-10000, -10000) // focus item scrolled off screen
        // Item top in viewport coords (reversed layout: viewportEnd - offset - size)
        val itemWindowY = (ls.layoutInfo.viewportEndOffset - itemInfo.offset - itemInfo.size).toFloat()
        val cr = focusCharRect
        val vp = viewportPosition
        // Convert from window coords to viewport-relative
        val charX = (if (draggingDown) cr.right else cr.left) - vp.x
        val charY = itemWindowY + (if (draggingDown) cr.bottom else cr.top) - vp.y
        // Anchor button corner at char corner with gap
        val x = if (draggingDown) charX else (charX - buttonSize.width).coerceAtLeast(0f)
        val y = if (draggingDown) charY + gap else charY - buttonSize.height - gap
        val clampedX = x.coerceIn(0f, (viewportWidth - buttonSize.width).coerceAtLeast(0f))
        return IntOffset(clampedX.toInt(), y.toInt())
    }

    fun startDragSelection(localStart: Offset, windowStart: Offset, focusRequester: FocusRequester) {
        val ls = listState?.value ?: return
        val idx = resolveIndexAtY(ls, localStart.y) ?: return
        startSelection(idx, windowStart.y, windowStart.x)
        focusWindowY = windowStart.y
        focusWindowX = windowStart.x
        try { focusRequester.requestFocus() } catch (_: Exception) {}
    }

    fun updateDragFocus(windowPos: Offset, localY: Float) {
        focusWindowY = windowPos.y
        focusWindowX = windowPos.x
        val ls = listState?.value ?: return
        val idx = resolveIndexAtY(ls, localY) ?: return
        updateFocusIndex(idx)
    }

    fun updateAutoScroll(draggingDown: Boolean, pointerY: Float, scope: CoroutineScope) {
        val edgeDistance = if (draggingDown) viewportBottom - pointerY else pointerY - viewportTop
        if (edgeDistance !in 0f..AUTO_SCROLL_ZONE_PX) {
            autoScrollJob?.cancel()
            autoScrollJob = null
            return
        }
        if (autoScrollJob?.isActive == true) return
        val ls = listState ?: return
        autoScrollJob = scope.launch {
            while (isActive && selectionState == SelectionState.Selecting) {
                val curEdge = if (draggingDown) viewportBottom - focusWindowY else focusWindowY - viewportTop
                if (curEdge >= AUTO_SCROLL_ZONE_PX) break
                val fraction = 1f - (curEdge / AUTO_SCROLL_ZONE_PX).coerceIn(0f, 1f)
                val speed = MIN_SCROLL_SPEED + (MAX_SCROLL_SPEED - MIN_SCROLL_SPEED) * fraction
                ls.value.scrollBy(if (draggingDown) -speed else speed)
                delay(16)
            }
        }
    }

    fun getSelectedCopiedText(revealedItems: Set<Long>, linkMode: SimplexLinkMode): String {
        val s = startIndex
        val e = endIndex
        if (s < 0 || e < 0) return ""
        val lo = minOf(s, e)
        val hi = maxOf(s, e)
        return (lo..hi).mapNotNull { idx ->
            val ci = items[idx].newest().item
            if (ci.meta.itemDeleted != null && (!revealedItems.contains(ci.id) || ci.isDeletedContent)) return@mapNotNull null
            val sel = selectedRange(idx) ?: return@mapNotNull null
            selectedItemCopiedText(ci, sel, linkMode)
        }.reversed().joinToString("\n")
    }

    // Returns the character range selected within the item at [index] in the current items list.
    // Offsets are cursor positions (between characters); selected characters are between min and max
    // cursors: range is min..(max - 1). In reversed layout: higher index = higher on screen.
    // startItemId/startOffset = anchor, endItemId/endOffset = focus.
    fun selectedRange(index: Int): IntRange? {
        val r = range ?: return null
        val s = startIndex
        val e = endIndex
        if (s < 0 || e < 0) return null
        val lo = minOf(s, e)
        val hi = maxOf(s, e)
        if (index < lo || index > hi) return null
        return when {
            // Single-item selection: characters between the two cursor positions
            index == s && index == e ->
                if (r.startOffset < 0 || r.endOffset < 0 || r.startOffset == r.endOffset) null
                else minOf(r.startOffset, r.endOffset) .. (maxOf(r.startOffset, r.endOffset) - 1)
            // Anchor item in multi-item selection: from cursor to end, or from start to cursor
            index == s ->
                if (r.startOffset < 0) null
                else if (s > e) r.startOffset until Int.MAX_VALUE
                else 0 until r.startOffset
            // Focus item in multi-item selection: symmetric to anchor
            index == e ->
                if (r.endOffset < 0) null
                else if (e < s) 0 until r.endOffset
                else r.endOffset until Int.MAX_VALUE
            // Interior items: fully selected
            else -> 0 until Int.MAX_VALUE
        }
    }
}

private fun List<MergedItem>.indexOfItemId(id: Long?): Int =
    if (id == null) -1 else indexOfFirst { it.newest().item.id == id }

// Extracts source text for the selected range within one item.
// Selection offsets are in display-text space. For transformed segments (mentions, links with showText),
// the full source is emitted if any part is selected. For untransformed segments, partial substring works.
private fun selectedItemCopiedText(ci: ChatItem, sel: IntRange, linkMode: SimplexLinkMode): String {
    val formattedText = ci.formattedText ?: return ci.text.substring(
        sel.first.coerceAtMost(ci.text.length),
        (sel.last + 1).coerceAtMost(ci.text.length)
    )
    val sb = StringBuilder()
    var displayOffset = 0
    for (ft in formattedText) {
        val segDisplay = itemSegmentDisplayText(ft, ci, linkMode)
        val displayEnd = displayOffset + segDisplay.length
        val overlapStart = maxOf(displayOffset, sel.first)
        val overlapEnd = minOf(displayEnd, sel.last + 1)
        if (overlapStart < overlapEnd) {
            if (ft.text.length == segDisplay.length) {
                sb.append(ft.text, overlapStart - displayOffset, overlapEnd - displayOffset)
            } else {
                sb.append(ft.text)
            }
        }
        displayOffset = displayEnd
    }
    return sb.toString()
}

// Snaps a boundary offset to include full transformed segments.
private fun snapOffset(ci: ChatItem, offset: Int, linkMode: SimplexLinkMode, expandRight: Boolean): Int {
    val formattedText = ci.formattedText ?: return offset
    var displayOffset = 0
    for (ft in formattedText) {
        val segDisplay = itemSegmentDisplayText(ft, ci, linkMode)
        val displayEnd = displayOffset + segDisplay.length
        if (offset > displayOffset && offset < displayEnd && ft.text.length != segDisplay.length) {
            return if (expandRight) displayEnd else displayOffset
        }
        displayOffset = displayEnd
    }
    return offset
}

val LocalSelectionManager = staticCompositionLocalOf<SelectionManager?> { null }

private const val AUTO_SCROLL_ZONE_PX = 40f
private const val MIN_SCROLL_SPEED = 2f
private const val MAX_SCROLL_SPEED = 20f

@Composable
fun BoxScope.SelectionHandler(
    manager: SelectionManager,
    listState: State<LazyListState>,
    mergedItems: State<MergedItems>,
    revealedItems: State<Set<Long>>,
    linkMode: SimplexLinkMode
): Modifier {
    val touchSlop = LocalViewConfiguration.current.touchSlop
    val clipboard = LocalClipboardManager.current
    val focusRequester = remember { FocusRequester() }
    val scope = rememberCoroutineScope()

    // Re-evaluate focus index on scroll during active drag
    LaunchedEffect(manager) {
        snapshotFlow { listState.value.firstVisibleItemScrollOffset }
            .collect {
                if (manager.selectionState == SelectionState.Selecting) {
                    val idx = resolveIndexAtY(listState.value, manager.focusWindowY - manager.viewportPosition.y)
                    if (idx != null) manager.updateFocusIndex(idx)
                }
            }
    }

    manager.listState = listState
    manager.onCopySelection = {
        clipboard.setText(AnnotatedString(manager.getSelectedCopiedText(revealedItems.value, linkMode)))
        showToast(generalGetString(MR.strings.copied))
    }

    // Keep the manager's view of the items list in sync, then clear the selection synchronously
    // (same frame, no visible flash) if either anchored item no longer exists — e.g. it was deleted.
    SideEffect {
        manager.items = mergedItems.value.items
        if (manager.range != null && (manager.startIndex < 0 || manager.endIndex < 0)) {
            manager.clearSelection()
        }
    }

    return Modifier
        .focusRequester(focusRequester)
        .focusable()
        .onKeyEvent { event ->
            if (manager.selectionState == SelectionState.Selected
                && (event.isCtrlPressed || event.isMetaPressed)
                && event.key == Key.C
                && event.type == KeyEventType.KeyDown
            ) {
                manager.onCopySelection?.invoke()
                true
            } else false
        }
        .onGloballyPositioned {
            val pos = it.positionInWindow()
            val bounds = it.boundsInWindow()
            manager.viewportTop = bounds.top
            manager.viewportBottom = bounds.bottom
            manager.viewportWidth = bounds.right - bounds.left
            manager.viewportHeight = bounds.bottom - bounds.top
            manager.viewportPosition = pos
        }
        .pointerInput(manager) {
            awaitEachGesture {
                var initialEvent: PointerInputChange
                // Wait for press, skip hovers
                do { initialEvent = awaitPointerEvent(PointerEventPass.Initial).changes.first() } while (!initialEvent.pressed)
                val localStart = initialEvent.position
                val windowStart = localStart + manager.viewportPosition
                if (manager.selectionState == SelectionState.Selected) initialEvent.consume()
                var totalDrag = Offset.Zero

                while (true) {
                    val event = awaitPointerEvent(PointerEventPass.Initial).changes.first()
                    when (manager.selectionState) {
                        SelectionState.Idle -> {
                            if (!event.pressed) return@awaitEachGesture
                            totalDrag += event.positionChange()
                            if (totalDrag.getDistance() > touchSlop) {
                                manager.startDragSelection(localStart, windowStart, focusRequester)
                                event.consume()
                            }
                        }
                        SelectionState.Selected -> {
                            if (!event.pressed) {
                                manager.clearSelection()
                                return@awaitEachGesture
                            }
                            event.consume()
                            totalDrag += event.positionChange()
                            if (totalDrag.getDistance() > touchSlop) {
                                manager.startDragSelection(localStart, windowStart, focusRequester)
                            }
                        }
                        SelectionState.Selecting -> {
                            if (!event.pressed) {
                                manager.endSelection()
                                manager.snapSelection(linkMode)
                                return@awaitEachGesture
                            }
                            val windowPos = event.position + manager.viewportPosition
                            manager.updateDragFocus(windowPos, event.position.y)
                            event.consume()
                            manager.updateAutoScroll(windowPos.y > windowStart.y, windowPos.y, scope)
                        }
                    }
                }
            }
        }
}

private fun resolveIndexAtY(listState: LazyListState, localY: Float): Int? {
    val reversedY = listState.layoutInfo.viewportEndOffset - localY
    val idx = listState.layoutInfo.visibleItemsInfo.find { item ->
        reversedY >= item.offset && reversedY < item.offset + item.size
    }?.index
    return idx
}

class ItemSelection(
    val highlightRange: IntRange?,
    val positionModifier: Modifier,
    val onTextLayoutResult: ((TextLayoutResult) -> Unit)?
)

// Sets up selection tracking for a text item: anchor/focus offset resolution,
// highlight range computation, and position/layout result capture.
@Composable
fun setupItemSelection(selectionManager: SelectionManager?, selectionIndex: Int, isLive: Boolean): ItemSelection {
    val boundsState = remember { mutableStateOf<Rect?>(null) }
    val layoutResultState = remember { mutableStateOf<TextLayoutResult?>(null) }

    if (selectionManager != null && selectionIndex >= 0 && !isLive) {
        val isAnchor = remember(selectionIndex) {
            derivedStateOf { selectionManager.startIndex == selectionIndex && selectionManager.selectionState == SelectionState.Selecting }
        }
        LaunchedEffect(isAnchor.value) {
            if (!isAnchor.value) return@LaunchedEffect
            val bounds = boundsState.value ?: return@LaunchedEffect
            val layout = layoutResultState.value ?: return@LaunchedEffect
            val offset = layout.getOffsetForPosition(
                Offset(selectionManager.anchorWindowX - bounds.left, selectionManager.anchorWindowY - bounds.top)
            )
            selectionManager.setAnchorOffset(offset)
        }

        val isFocus = remember(selectionIndex) {
            derivedStateOf { selectionManager.endIndex == selectionIndex && selectionManager.selectionState == SelectionState.Selecting }
        }
        if (isFocus.value) {
            LaunchedEffect(Unit) {
                snapshotFlow { selectionManager.focusWindowY to selectionManager.focusWindowX }
                    .collect { (py, px) ->
                        val bounds = boundsState.value ?: return@collect
                        val layout = layoutResultState.value ?: return@collect
                        val offset = layout.getOffsetForPosition(Offset(px - bounds.left, py - bounds.top))
                        val charBox = layout.getBoundingBox(offset.coerceIn(0, layout.layoutInput.text.length - 1))
                        val ls = selectionManager.listState?.value
                        val itemInfo = ls?.layoutInfo?.visibleItemsInfo?.find { it.index == selectionIndex }
                        val charRect = if (ls != null && itemInfo != null) {
                            val itemWindowY = (ls.layoutInfo.viewportEndOffset - itemInfo.offset - itemInfo.size).toFloat()
                            Rect(
                                left = bounds.left + charBox.left,
                                top = bounds.top + charBox.top - itemWindowY,
                                right = bounds.left + charBox.right,
                                bottom = bounds.top + charBox.bottom - itemWindowY
                            )
                        } else Rect.Zero
                        selectionManager.updateFocusOffset(offset, charRect)
                    }
            }
        }
    }

    val highlightRange = if (selectionManager != null && selectionIndex >= 0) {
        remember(selectionIndex) { derivedStateOf { selectionManager.selectedRange(selectionIndex) } }.value
    } else null

    val positionModifier = if (selectionManager != null) {
        Modifier.onGloballyPositioned {
            val pos = it.positionInWindow()
            boundsState.value = Rect(pos.x, pos.y, pos.x + it.size.width, pos.y + it.size.height)
        }
    } else Modifier

    val onTextLayoutResult: ((TextLayoutResult) -> Unit)? = if (selectionManager != null) {
        { layoutResultState.value = it }
    } else null

    return ItemSelection(highlightRange, positionModifier, onTextLayoutResult)
}

// Sets up full-item selection for emoji items (no character-level tracking).
@Composable
fun setupEmojiSelection(selectionManager: SelectionManager?, selectionIndex: Int, textLength: Int): Boolean {
    if (selectionManager == null || selectionIndex < 0) return false

    val isAnchor = remember(selectionIndex) {
        derivedStateOf { selectionManager.startIndex == selectionIndex && selectionManager.selectionState == SelectionState.Selecting }
    }
    LaunchedEffect(isAnchor.value) {
        if (!isAnchor.value) return@LaunchedEffect
        selectionManager.setAnchorOffset(0)
    }

    val isFocus = remember(selectionIndex) {
        derivedStateOf { selectionManager.endIndex == selectionIndex && selectionManager.selectionState == SelectionState.Selecting }
    }
    if (isFocus.value) {
        LaunchedEffect(Unit) {
            snapshotFlow { selectionManager.focusWindowY }
                .collect { selectionManager.updateFocusOffset(textLength) }
        }
    }

    return remember(selectionIndex) { derivedStateOf { selectionManager.selectedRange(selectionIndex) != null } }.value
}

@Composable
fun SelectionCopyButton() {
    val manager = LocalSelectionManager.current ?: return
    val range = manager.range ?: return
    if (manager.selectionState != SelectionState.Selected || manager.focusCharRect == Rect.Zero) return
    val s = manager.startIndex
    val e = manager.endIndex
    if (s < 0 || e < 0) return
    val draggingDown = s > e || (s == e && range.startOffset < range.endOffset)
    val gap = with(LocalDensity.current) { 4.dp.toPx() }
    var buttonSize by remember { mutableStateOf(IntSize.Zero) }
    Row(
        Modifier
            .offset { manager.copyButtonOffset(draggingDown, gap, buttonSize) }
            .onSizeChanged { buttonSize = it }
            .background(MaterialTheme.colors.surface, RoundedCornerShape(20.dp))
            .border(1.dp, MaterialTheme.colors.onSurface.copy(alpha = 0.12f), RoundedCornerShape(20.dp))
            .clip(RoundedCornerShape(20.dp))
            .clickable {
                manager.onCopySelection?.invoke()
                manager.clearSelection()
            }
            .padding(horizontal = 16.dp, vertical = 8.dp),
        verticalAlignment = Alignment.CenterVertically
    ) {
        Icon(painterResource(MR.images.ic_content_copy), null, Modifier.size(16.dp), tint = MaterialTheme.colors.primary)
        Spacer(Modifier.width(6.dp))
        Text(generalGetString(MR.strings.copy_verb), color = MaterialTheme.colors.primary)
    }
}
