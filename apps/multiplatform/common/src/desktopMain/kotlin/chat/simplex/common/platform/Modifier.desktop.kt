package chat.simplex.common.platform

import androidx.compose.foundation.contextMenuOpenDetector
import androidx.compose.foundation.draganddrop.dragAndDropTarget
import androidx.compose.runtime.Composable
import androidx.compose.runtime.remember
import androidx.compose.runtime.snapshots.Snapshot
import androidx.compose.ui.*
import androidx.compose.ui.autofill.ContentType
import androidx.compose.ui.draganddrop.*
import androidx.compose.ui.draganddrop.DragData
import androidx.compose.ui.input.pointer.*
import androidx.compose.ui.layout.AlignmentLine
import androidx.compose.ui.layout.FirstBaseline
import androidx.compose.ui.layout.LastBaseline
import androidx.compose.ui.layout.layout
import androidx.compose.ui.text.TextLayoutResult
import androidx.compose.ui.text.style.LineHeightStyle
import chat.simplex.common.ui.theme.Inter
import chat.simplex.common.views.chat.item.LocalBubbleLayout
import chat.simplex.res.MR
import org.jetbrains.skia.FontMgr
import org.jetbrains.skia.Typeface
import java.awt.Image
import java.awt.datatransfer.DataFlavor
import java.awt.datatransfer.Transferable
import java.awt.image.BufferedImage
import java.io.File
import java.net.URI
import kotlin.math.abs
import kotlin.math.floor
import kotlin.math.roundToInt
import org.jetbrains.skia.Font as SkFont

@Composable
actual fun Modifier.desktopOnExternalDrag(
  enabled: Boolean,
  onFiles: (List<File>) -> Unit,
  onImage: (File) -> Unit,
  onText: (String) -> Unit
): Modifier {
  val callback = remember {
    object : DragAndDropTarget {
      override fun onDrop(event: DragAndDropEvent): Boolean {
        when (val data = event.dragData()) {
          // data.readFiles() returns filePath in URI format (where spaces replaces with %20). But it's an error-prone idea to work later
          // with such format when everywhere we use absolutePath in File() format
          is DragData.FilesList -> {
            val files = data.readFiles()
            // When dragging and dropping an image from browser, it comes to FilesList section but no files inside
            if (files.isNotEmpty()) {
              onFiles(files.map { URI.create(it).toFile() })
            } else {
              try {
                val transferable = event.awtTransferable
                if (transferable.isDataFlavorSupported(DataFlavor.imageFlavor)) {
                  onImage(DragDataImageImpl(transferable).bufferedImage().saveInTmpFile() ?: return false)
                } else {
                  return false
                }
              } catch (e: Exception) {
                Log.e(TAG, e.stackTraceToString())
                return false
              }
            }
          }
          is DragData.Image -> onImage(DragDataImageImpl(event.awtTransferable).bufferedImage().saveInTmpFile() ?: return false)
          is DragData.Text -> onText(data.readText())
        }
        return true
      }
    }
  }
  return dragAndDropTarget(shouldStartDragAndDrop = { true }, target = callback)
}

// Copied from AwtDragData and modified
private class DragDataImageImpl(private val transferable: Transferable) {
  fun bufferedImage(): BufferedImage = (transferable.getTransferData(DataFlavor.imageFlavor) as Image).bufferedImage()
  private fun Image.bufferedImage(): BufferedImage {
    if (this is BufferedImage && hasAlpha()) {
      // Such image cannot be drawn as JPG, only PNG
      return this
    }
    // Creating non-transparent image which can be drawn as JPG
    val bufferedImage = BufferedImage(getWidth(null), getHeight(null), BufferedImage.TYPE_INT_RGB)
    val g2 = bufferedImage.createGraphics()
    try {
      g2.drawImage(this, 0, 0, null)
    } finally {
      g2.dispose()
    }
    return bufferedImage
  }
}

actual fun Modifier.onRightClick(action: () -> Unit): Modifier = contextMenuOpenDetector { action() }

actual fun Modifier.desktopPointerHoverIconHand(): Modifier = Modifier.pointerHoverIcon(PointerIcon.Hand)

actual fun Modifier.desktopOnHovered(action: (Boolean) -> Unit): Modifier =
  this then Modifier
    .onPointerEvent(PointerEventType.Enter) { action(true) }
    .onPointerEvent(PointerEventType.Exit) { action(false) }

@Composable
actual fun Modifier.desktopDrawnBaselines(textLayout: () -> TextLayoutResult?): Modifier =
  if (LocalBubbleLayout.current) {
    layout { measurable, constraints ->
      val placeable = measurable.measure(constraints)
      val result = Snapshot.withoutReadObservation(textLayout)
      val first = placeable[FirstBaseline]
      val last = placeable[LastBaseline]
      val baselines = if (result == null || first == AlignmentLine.Unspecified || last == AlignmentLine.Unspecified) {
        emptyMap()
      } else {
        mapOf(
          FirstBaseline to first + drawnBaselineShift(result, 0),
          LastBaseline to last + drawnBaselineShift(result, result.lineCount - 1)
        )
      }
      layout(placeable.width, placeable.height, baselines) { placeable.place(0, 0) }
    }
  } else {
    this
  }

private val interTypeface: Typeface? by lazy { FontMgr.default.makeFromFile(MR.fonts.inter_regular.file.absolutePath) }

private fun drawnBaselineShift(layout: TextLayoutResult, line: Int): Int {
  val style = layout.layoutInput.style
  val typeface = interTypeface
  if (style.fontFamily != Inter || !style.fontSize.isSp || !style.lineHeight.isSp || typeface == null) return 0
  val density = layout.layoutInput.density
  val fontSize = with(density) { style.fontSize.toPx() }
  val lineHeight = with(density) { style.lineHeight.toPx() }
  val metrics = SkFont(typeface, fontSize).use { it.metrics }
  val fontHeight = (metrics.descent - metrics.ascent + metrics.leading).roundToInt().toFloat()
  val topRatio = when (style.lineHeightStyle?.alignment) {
    LineHeightStyle.Alignment.Top -> 0f
    LineHeightStyle.Alignment.Center -> 0.5f
    LineHeightStyle.Alignment.Bottom -> 1f
    else -> -1f
  }
  val (emptyAscent, emptyDescent) = if (topRatio in 0f..1f) {
    val extraLeading = lineHeight - fontHeight
    metrics.ascent - extraLeading * topRatio to metrics.descent + extraLeading * (1 - topRatio)
  } else {
    val multiplier = lineHeight / fontHeight
    metrics.ascent * multiplier to metrics.descent * multiplier
  }
  val paragraph = layout.multiParagraph
  val lineTop = (0 until line).sumOf { paragraph.getLineHeight(it).toDouble() }.toFloat()
  val reported = layout.getLineBaseline(line)
  val descent = lineTop + paragraph.getLineHeight(line) - reported
  val ascent = if (abs(descent - emptyDescent) < 0.01f) emptyAscent else descent - lineHeight
  val drawn = lineTop + floor(metrics.leading / 2 - ascent + 0.5f)
  return drawn.roundToInt() - reported.roundToInt()
}

actual fun newPasswordContentType(): ContentType = ContentType.NewPassword
