package chat.simplex.common.views.chat

import androidx.compose.foundation.gestures.awaitEachGesture
import androidx.compose.foundation.gestures.scrollBy
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
