package chat.simplex.common.views.chat

import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.geometry.Rect
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.TextLayoutResult
import androidx.compose.ui.unit.dp
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource

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

@Composable
fun SelectionCopyButton(onCopy: () -> Unit) {
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
        Text(generalGetString(MR.strings.copy_verb), color = MaterialTheme.colors.primary)
    }
}
