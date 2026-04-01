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
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.focus.focusRequester
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.input.key.*
import androidx.compose.ui.input.pointer.*
import androidx.compose.ui.layout.boundsInWindow
import androidx.compose.ui.layout.onGloballyPositioned
import androidx.compose.ui.layout.positionInWindow
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.platform.LocalViewConfiguration
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.platform.Log
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.views.chat.item.displayText
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import kotlinx.coroutines.*

private const val TAG_SEL = "TextSelection"

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

    var focusWindowY by mutableStateOf(0f)
    var focusWindowX by mutableStateOf(0f)

    fun startSelection(startIndex: Int) {
        range = SelectionRange(startIndex, 0, startIndex, 0)
        selectionState = SelectionState.Selecting
    }

    fun setAnchorOffset(offset: Int) {
        val r = range ?: return
        range = r.copy(startOffset = offset)
    }

    fun updateFocusIndex(index: Int) {
        val r = range ?: return
        range = r.copy(endIndex = index)
    }

    fun updateFocusOffset(offset: Int) {
        val r = range ?: return
        range = r.copy(endOffset = offset)
    }

    fun endSelection() {
        selectionState = SelectionState.Selected
    }

    fun clearSelection() {
        range = null
        selectionState = SelectionState.Idle
    }

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
}

fun highlightedRange(range: SelectionRange?, index: Int): IntRange? {
    val r = range ?: return null
    val lo = minOf(r.startIndex, r.endIndex)
    val hi = maxOf(r.startIndex, r.endIndex)
    if (index < lo || index > hi) return null
    return when {
        index == r.startIndex && index == r.endIndex ->
            if (r.startOffset == r.endOffset) null
            else minOf(r.startOffset, r.endOffset) until maxOf(r.startOffset, r.endOffset)
        index == r.startIndex ->
            if (r.startIndex > r.endIndex) r.startOffset until Int.MAX_VALUE
            else 0 until r.startOffset
        index == r.endIndex ->
            if (r.endIndex < r.startIndex) 0 until r.endOffset
            else r.endOffset until Int.MAX_VALUE
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

    // Copy button
    if (manager.selectionState == SelectionState.Selected) {
        SelectionCopyButton(
            onCopy = {
                clipboard.setText(AnnotatedString(manager.getSelectedText(mergedItems.value.items, linkMode)))
            }
        )
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
                clipboard.setText(AnnotatedString(manager.getSelectedText(mergedItems.value.items, linkMode)))
                true
            } else false
        }
        .onGloballyPositioned {
            positionInWindow = it.positionInWindow()
            val bounds = it.boundsInWindow()
            viewportTop = bounds.top
            viewportBottom = bounds.bottom
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
                        Log.d(TAG_SEL, "dragStart localStart=$localStart windowStart=$windowStart idx=$idx")
                        if (idx != null) {
                            manager.startSelection(idx)
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
                            if (idx != manager.range?.endIndex) Log.d(TAG_SEL, "focusIndexChanged idx=$idx range=${manager.range}")
                            manager.updateFocusIndex(idx)
                        }

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
                                while (isActive && manager.selectionState == SelectionState.Selecting) {
                                    val curEdge = if (draggingDown) {
                                        viewportBottom - manager.focusWindowY
                                    } else {
                                        manager.focusWindowY - viewportTop
                                    }
                                    if (curEdge >= AUTO_SCROLL_ZONE_PX) break

                                    val fraction = 1f - (curEdge / AUTO_SCROLL_ZONE_PX).coerceIn(0f, 1f)
                                    val speed = MIN_SCROLL_SPEED + (MAX_SCROLL_SPEED - MIN_SCROLL_SPEED) * fraction
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
}

private fun resolveIndexAtY(listState: LazyListState, localY: Float): Int? {
    val reversedY = listState.layoutInfo.viewportEndOffset - localY
    val idx = listState.layoutInfo.visibleItemsInfo.find { item ->
        reversedY >= item.offset && reversedY < item.offset + item.size
    }?.index
    Log.d(TAG_SEL, "resolveIndexAtY localY=$localY reversedY=$reversedY → index=$idx")
    return idx
}

@Composable
private fun BoxScope.SelectionCopyButton(onCopy: () -> Unit) {
    Row(
        Modifier
            .align(Alignment.BottomCenter)
            .padding(bottom = 8.dp)
            .background(MaterialTheme.colors.surface, RoundedCornerShape(20.dp))
            .border(1.dp, MaterialTheme.colors.onSurface.copy(alpha = 0.12f), RoundedCornerShape(20.dp))
            .clickable { onCopy() }
            .padding(horizontal = 16.dp, vertical = 8.dp),
        verticalAlignment = Alignment.CenterVertically
    ) {
        Icon(painterResource(MR.images.ic_content_copy), null, Modifier.size(16.dp), tint = MaterialTheme.colors.primary)
        Spacer(Modifier.width(6.dp))
        Text(generalGetString(MR.strings.copy_verb), color = MaterialTheme.colors.primary)
    }
}
