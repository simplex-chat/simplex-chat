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
import androidx.compose.ui.geometry.Rect
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.input.key.*
import androidx.compose.ui.input.pointer.*
import androidx.compose.ui.layout.boundsInWindow
import androidx.compose.ui.layout.onGloballyPositioned
import androidx.compose.ui.layout.positionInWindow
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.platform.LocalViewConfiguration
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.TextLayoutResult
import androidx.compose.ui.unit.dp
import chat.simplex.common.platform.Log
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import kotlinx.coroutines.*

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

enum class SelectionState { Idle, Selecting, Selected }

class SelectionManager {
    var selectionState by mutableStateOf(SelectionState.Idle)
        private set

    var coords by mutableStateOf<SelectionCoords?>(null)
        private set

    var lastPointerWindowY: Float = 0f
        private set
    var lastPointerWindowX: Float = 0f
        private set

    private val participants = mutableListOf<SelectionParticipant>()
    val captured = mutableStateMapOf<Long, CapturedText>()

    fun register(participant: SelectionParticipant) {
        participants.add(participant)
        if (selectionState == SelectionState.Selecting) {
            coords?.let { recomputeParticipant(participant, it) }
        }
    }

    fun unregister(participant: SelectionParticipant) {
        participants.remove(participant)
    }

    fun startSelection(startY: Float, startX: Float) {
        coords = SelectionCoords(startY, startX, startY, startX)
        selectionState = SelectionState.Selecting
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
        selectionState = SelectionState.Selected
    }

    fun clearSelection() {
        coords = null
        selectionState = SelectionState.Idle
        captured.clear()
    }

    private fun recomputeAll() {
        val c = coords ?: return
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

        visibleOutOfRange.forEach { captured.remove(it) }

        for ((_, p) in visibleInRange) {
            recomputeParticipant(p, c)
        }
    }

    private fun recomputeParticipant(participant: SelectionParticipant, coords: SelectionCoords) {
        val bounds = participant.getYBounds() ?: return
        Log.d("TextSelection", "recompute item=${participant.itemId} bounds=${bounds.start}..${bounds.endInclusive} coords.topY=${coords.topY} coords.bottomY=${coords.bottomY}")
        val highlightRange = participant.calculateHighlightRange(coords) ?: return
        Log.d("TextSelection", "  highlightRange=$highlightRange selectableEnd=${participant.getSelectableEnd()}")
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

private const val AUTO_SCROLL_ZONE_PX = 40f
private const val MIN_SCROLL_SPEED = 2f
private const val MAX_SCROLL_SPEED = 20f

/**
 * Composable that installs selection effects and returns a Modifier for the LazyColumn.
 * Also emits the copy button UI in the BoxScope.
 */
@Composable
fun BoxScope.SelectionHandler(
    manager: SelectionManager?,
    listState: State<LazyListState>
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

    // Re-evaluate selection on scroll (handles mouse wheel and auto-scroll)
    LaunchedEffect(manager) {
        snapshotFlow { listState.value.firstVisibleItemScrollOffset }
            .collect {
                if (manager.selectionState == SelectionState.Selecting) {
                    manager.updateSelection(
                        manager.lastPointerWindowY,
                        manager.lastPointerWindowX
                    )
                }
            }
    }

    // Copy button
    if (manager.selectionState == SelectionState.Selected) {
        SelectionCopyButton(
            onCopy = {
                clipboard.setText(AnnotatedString(manager.getSelectedText()))
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
                clipboard.setText(AnnotatedString(manager.getSelectedText()))
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
                if (!firstChange.pressed) return@awaitEachGesture // skip hover, scroll

                val wasSelected = manager.selectionState == SelectionState.Selected
                if (wasSelected) firstChange.consume() // prevent link/menu activation on click-to-clear

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
                            manager.endSelection() // Selecting → Selected
                        } else if (wasSelected) {
                            manager.clearSelection() // Selected → Idle
                        }
                        // Idle + click: do nothing, event passed through to children
                        break
                    }

                    totalDrag += change.positionChange()

                    if (!isDragging && totalDrag.getDistance() > touchSlop) {
                        isDragging = true
                        Log.d("TextSelection", "startSelection: localStart=$localStart posInWindow=$positionInWindow windowStart=$windowStart")
                        manager.startSelection(windowStart.y, windowStart.x) // → Selecting
                        try { focusRequester.requestFocus() } catch (_: Exception) {}
                        change.consume()
                    }

                    if (isDragging) {
                        val windowPos = change.position + positionInWindow
                        manager.updateSelection(windowPos.y, windowPos.x)
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
                                        viewportBottom - manager.lastPointerWindowY
                                    } else {
                                        manager.lastPointerWindowY - viewportTop
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
