package chat.simplex.common.views.chat

import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.clickable
import androidx.compose.foundation.focusable
import androidx.compose.foundation.gestures.awaitEachGesture
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
import chat.simplex.common.views.chat.item.displayText
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import kotlinx.coroutines.*

val SelectionHighlightColor = Color(0x4D0066FF)

data class ItemContext(
    val selectionIndex: Int = -1
)

val LocalItemContext = compositionLocalOf { ItemContext() }

data class SelectionRange(
    val startIndex: Int,
    val startOffset: Int,
    val endIndex: Int,
    val endOffset: Int
)

enum class SelectionState { Idle, Selecting, Selected }

class SelectionManager {
    var selectionState by mutableStateOf(SelectionState.Idle)
        private set

    var range by mutableStateOf<SelectionRange?>(null)
        private set

    var anchorWindowY by mutableStateOf(0f)
        private set
    var anchorWindowX by mutableStateOf(0f)
        private set
    var focusWindowY by mutableStateOf(0f)
    var focusWindowX by mutableStateOf(0f)
    var viewportWidth by mutableStateOf(0f)
    var viewportHeight by mutableStateOf(0f)
    var viewportPosition by mutableStateOf(Offset.Zero)
    var focusCharRect by mutableStateOf(Rect.Zero) // X: absolute window, Y: relative to item
    var listState: State<LazyListState>? = null
    var onCopySelection: (() -> Unit)? = null

    fun startSelection(startIndex: Int, anchorY: Float, anchorX: Float) {
        range = SelectionRange(startIndex, -1, startIndex, -1)
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
        range = r.copy(endIndex = index)
    }

    fun updateFocusOffset(offset: Int, charRect: Rect = Rect.Zero) {
        val r = range ?: return
        range = r.copy(endOffset = offset)
        focusCharRect = charRect
    }

    fun endSelection() {
        selectionState = SelectionState.Selected
    }

    fun clearSelection() {
        range = null
        selectionState = SelectionState.Idle
    }

    fun copyButtonOffset(draggingDown: Boolean, gap: Float, buttonSize: IntSize): IntOffset {
        val r = range ?: return IntOffset.Zero
        val ls = listState?.value ?: return IntOffset.Zero
        val itemInfo = ls.layoutInfo.visibleItemsInfo.find { it.index == r.endIndex }
            ?: return IntOffset(-10000, -10000)
        val itemWindowY = (ls.layoutInfo.viewportEndOffset - itemInfo.offset - itemInfo.size).toFloat()
        val cr = focusCharRect
        val vp = viewportPosition
        val charX = (if (draggingDown) cr.right else cr.left) - vp.x
        val charY = itemWindowY + (if (draggingDown) cr.bottom else cr.top) - vp.y
        val x = if (draggingDown) charX else (charX - buttonSize.width).coerceAtLeast(0f)
        val y = if (draggingDown) charY + gap else charY - buttonSize.height - gap
        val clampedX = x.coerceIn(0f, (viewportWidth - buttonSize.width).coerceAtLeast(0f))
        return IntOffset(clampedX.toInt(), y.toInt())
    }

    fun getSelectedText(items: List<MergedItem>, linkMode: SimplexLinkMode): String {
        val r = range ?: return ""
        val lo = minOf(r.startIndex, r.endIndex)
        val hi = maxOf(r.startIndex, r.endIndex)
        return (lo..hi).mapNotNull { idx ->
            val ci = items.getOrNull(idx)?.newest()?.item ?: return@mapNotNull null
            val sel = selectedRange(range, idx) ?: return@mapNotNull null
            val text = displayText(ci, linkMode, sendCommandMsg = false)
            text.substring(
                sel.first.coerceAtMost(text.length),
                (sel.last + 1).coerceAtMost(text.length)
            )
        }.reversed().joinToString("\n")
    }
}

// Returns the character range selected within a given item.
// Offsets are cursor positions (between characters), so the selected characters
// are those between min and max cursors: range is min..(max - 1).
// In reversed layout: higher index = higher on screen.
// startIndex/startOffset = anchor, endIndex/endOffset = focus.
fun selectedRange(range: SelectionRange?, index: Int): IntRange? {
    val r = range ?: return null
    val lo = minOf(r.startIndex, r.endIndex)
    val hi = maxOf(r.startIndex, r.endIndex)
    if (index < lo || index > hi) return null
    return when {
        // Single-item selection: characters between the two cursor positions
        index == r.startIndex && index == r.endIndex ->
            if (r.startOffset < 0 || r.endOffset < 0 || r.startOffset == r.endOffset) null
            else minOf(r.startOffset, r.endOffset) .. (maxOf(r.startOffset, r.endOffset) - 1)
        // Anchor item in multi-item selection: from cursor to end, or from start to cursor
        index == r.startIndex ->
            if (r.startOffset < 0) null
            else if (r.startIndex > r.endIndex) r.startOffset until Int.MAX_VALUE
            else 0 until r.startOffset
        // Focus item in multi-item selection: symmetric to anchor
        index == r.endIndex ->
            if (r.endOffset < 0) null
            else if (r.endIndex < r.startIndex) 0 until r.endOffset
            else r.endOffset until Int.MAX_VALUE
        // Interior items: fully selected
        else -> 0 until Int.MAX_VALUE
    }
}

val LocalSelectionManager = staticCompositionLocalOf<SelectionManager?> { null }

private const val AUTO_SCROLL_ZONE_PX = 40f
private const val MIN_SCROLL_SPEED = 2f
private const val MAX_SCROLL_SPEED = 20f

@Composable
fun BoxScope.SelectionHandler(
    manager: SelectionManager?,
    listState: State<LazyListState>,
    mergedItems: State<MergedItems>,
    linkMode: SimplexLinkMode
): Modifier {
    if (manager == null || !appPlatform.isDesktop) return Modifier

    val touchSlop = LocalViewConfiguration.current.touchSlop
    val clipboard = LocalClipboardManager.current
    val focusRequester = remember { FocusRequester() }
    var positionInWindow by remember { mutableStateOf(Offset.Zero) }
    var viewportTop by remember { mutableStateOf(0f) }
    var viewportBottom by remember { mutableStateOf(0f) }
    val scope = rememberCoroutineScope()
    var autoScrollJob by remember { mutableStateOf<Job?>(null) }

    // Re-evaluate focus index on scroll during active drag
    LaunchedEffect(manager) {
        snapshotFlow { listState.value.firstVisibleItemScrollOffset }
            .collect {
                if (manager.selectionState == SelectionState.Selecting) {
                    val idx = resolveIndexAtY(listState.value, manager.focusWindowY - positionInWindow.y)
                    if (idx != null) manager.updateFocusIndex(idx)
                }
            }
    }

    manager.listState = listState
    manager.onCopySelection = {
        clipboard.setText(AnnotatedString(manager.getSelectedText(mergedItems.value.items, linkMode)))
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
            positionInWindow = it.positionInWindow()
            val bounds = it.boundsInWindow()
            viewportTop = bounds.top
            viewportBottom = bounds.bottom
            manager.viewportWidth = bounds.right - bounds.left
            manager.viewportHeight = bounds.bottom - bounds.top
            manager.viewportPosition = positionInWindow
        }
        .pointerInput(manager) {
            awaitEachGesture {
                val down = awaitPointerEvent(PointerEventPass.Initial)
                val firstChange = down.changes.first()
                if (!firstChange.pressed) return@awaitEachGesture

                val wasSelected = manager.selectionState == SelectionState.Selected
                if (wasSelected) firstChange.consume()

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
                            manager.endSelection()
                        } else if (wasSelected) {
                            manager.clearSelection()
                        }
                        break
                    }

                    totalDrag += change.positionChange()

                    if (!isDragging && totalDrag.getDistance() > touchSlop) {
                        isDragging = true
                        val idx = resolveIndexAtY(listState.value, localStart.y)
                        Log.e(TAG, "dragStart localStart=$localStart windowStart=$windowStart idx=$idx")
                        if (idx != null) {
                            manager.startSelection(idx, windowStart.y, windowStart.x)
                            manager.focusWindowY = windowStart.y
                            manager.focusWindowX = windowStart.x
                        }
                        try { focusRequester.requestFocus() } catch (_: Exception) {}
                        change.consume()
                    }

                    if (isDragging) {
                        val windowPos = change.position + positionInWindow
                        manager.focusWindowY = windowPos.y
                        manager.focusWindowX = windowPos.x

                        val idx = resolveIndexAtY(listState.value, change.position.y)
                        if (idx != null) {
                            if (idx != manager.range?.endIndex) Log.e(TAG, "focusIndexChanged idx=$idx range=${manager.range}")
                            manager.updateFocusIndex(idx)
                        }

                        change.consume()

                        val draggingDown = windowPos.y > windowStart.y
                        autoScrollJob = updateAutoScroll(
                            draggingDown, windowPos.y, viewportTop, viewportBottom,
                            autoScrollJob, scope, manager, listState
                        )
                    }
                }
            }
        }
}

private fun updateAutoScroll(
    draggingDown: Boolean, pointerY: Float, viewportTop: Float, viewportBottom: Float,
    currentJob: Job?, scope: CoroutineScope, manager: SelectionManager, listState: State<LazyListState>
): Job? {
    val edgeDistance = if (draggingDown) viewportBottom - pointerY else pointerY - viewportTop
    if (edgeDistance !in 0f..AUTO_SCROLL_ZONE_PX) {
        currentJob?.cancel()
        return null
    }
    if (currentJob?.isActive == true) return currentJob
    return scope.launch {
        while (isActive && manager.selectionState == SelectionState.Selecting) {
            val curEdge = if (draggingDown) viewportBottom - manager.focusWindowY else manager.focusWindowY - viewportTop
            if (curEdge >= AUTO_SCROLL_ZONE_PX) break
            val fraction = 1f - (curEdge / AUTO_SCROLL_ZONE_PX).coerceIn(0f, 1f)
            val speed = MIN_SCROLL_SPEED + (MAX_SCROLL_SPEED - MIN_SCROLL_SPEED) * fraction
            listState.value.scrollBy(if (draggingDown) -speed else speed)
            delay(16)
        }
    }
}

private fun resolveIndexAtY(listState: LazyListState, localY: Float): Int? {
    val reversedY = listState.layoutInfo.viewportEndOffset - localY
    val idx = listState.layoutInfo.visibleItemsInfo.find { item ->
        reversedY >= item.offset && reversedY < item.offset + item.size
    }?.index
    Log.e(TAG, "resolveIndexAtY localY=$localY reversedY=$reversedY → index=$idx")
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
            derivedStateOf { selectionManager.range?.startIndex == selectionIndex && selectionManager.selectionState == SelectionState.Selecting }
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
            derivedStateOf { selectionManager.range?.endIndex == selectionIndex && selectionManager.selectionState == SelectionState.Selecting }
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
        remember(selectionIndex) { derivedStateOf { selectedRange(selectionManager.range, selectionIndex) } }.value
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
        derivedStateOf { selectionManager.range?.startIndex == selectionIndex && selectionManager.selectionState == SelectionState.Selecting }
    }
    LaunchedEffect(isAnchor.value) {
        if (!isAnchor.value) return@LaunchedEffect
        selectionManager.setAnchorOffset(0)
    }

    val isFocus = remember(selectionIndex) {
        derivedStateOf { selectionManager.range?.endIndex == selectionIndex && selectionManager.selectionState == SelectionState.Selecting }
    }
    if (isFocus.value) {
        LaunchedEffect(Unit) {
            snapshotFlow { selectionManager.focusWindowY }
                .collect { selectionManager.updateFocusOffset(textLength) }
        }
    }

    return remember(selectionIndex) { derivedStateOf { selectedRange(selectionManager.range, selectionIndex) != null } }.value
}

@Composable
fun SelectionCopyButton() {
    val manager = LocalSelectionManager.current ?: return
    val range = manager.range ?: return
    if (manager.selectionState != SelectionState.Selected || manager.focusCharRect == Rect.Zero) return
    val draggingDown = range.startIndex > range.endIndex || (range.startIndex == range.endIndex && range.startOffset < range.endOffset)
    val gap = with(LocalDensity.current) { 4.dp.toPx() }
    var buttonSize by remember { mutableStateOf(IntSize.Zero) }
    Row(
        Modifier
            .offset { manager.copyButtonOffset(draggingDown, gap, buttonSize) }
            .onSizeChanged { buttonSize = it }
            .background(MaterialTheme.colors.surface, RoundedCornerShape(20.dp))
            .border(1.dp, MaterialTheme.colors.onSurface.copy(alpha = 0.12f), RoundedCornerShape(20.dp))
            .clip(RoundedCornerShape(20.dp))
            .clickable { manager.onCopySelection?.invoke() }
            .padding(horizontal = 16.dp, vertical = 8.dp),
        verticalAlignment = Alignment.CenterVertically
    ) {
        Icon(painterResource(MR.images.ic_content_copy), null, Modifier.size(16.dp), tint = MaterialTheme.colors.primary)
        Spacer(Modifier.width(6.dp))
        Text(generalGetString(MR.strings.copy_verb), color = MaterialTheme.colors.primary)
    }
}
