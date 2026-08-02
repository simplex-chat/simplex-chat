package chat.simplex.common.platform

import androidx.compose.foundation.contextMenuOpenDetector
import androidx.compose.foundation.draganddrop.dragAndDropTarget
import androidx.compose.runtime.Composable
import androidx.compose.runtime.remember
import androidx.compose.ui.*
import androidx.compose.ui.draganddrop.*
import androidx.compose.ui.draganddrop.DragData
import androidx.compose.ui.input.pointer.*
import java.awt.Image
import java.awt.Cursor
import java.awt.datatransfer.DataFlavor
import java.awt.datatransfer.Transferable
import java.awt.image.BufferedImage
import java.io.File
import java.net.URI

@Composable
actual fun Modifier.desktopOnExternalDrag(
  enabled: Boolean,
  onFiles: (List<File>) -> Unit,
  onImage: (File) -> Unit,
  onText: (String) -> Unit,
  onDragging: (Boolean) -> Unit,
): Modifier {
  val callback = remember(enabled, onFiles, onImage, onText, onDragging) {
    object : DragAndDropTarget {
      override fun onStarted(event: DragAndDropEvent) { onDragging(enabled) }
      override fun onEntered(event: DragAndDropEvent) { onDragging(enabled) }
      override fun onExited(event: DragAndDropEvent) { onDragging(false) }
      override fun onEnded(event: DragAndDropEvent) { onDragging(false) }

      override fun onDrop(event: DragAndDropEvent): Boolean {
        if (!enabled) return false
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
  return dragAndDropTarget(shouldStartDragAndDrop = { enabled }, target = callback)
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

actual fun Modifier.desktopPointerHoverIconResize(): Modifier =
  this then Modifier.pointerHoverIcon(PointerIcon(Cursor.getPredefinedCursor(Cursor.E_RESIZE_CURSOR)))

actual val macOSWindowVibrancyAvailable: Boolean = desktopPlatform.isMac()

actual fun openSystemNotificationSettings() {
  if (desktopPlatform.isMac()) chat.simplex.common.model.MacOSNotifications.openSettings()
}

@Composable
actual fun Modifier.desktopMessageSelection(enabled: Boolean, onToggle: () -> Unit, onRange: () -> Unit): Modifier =
  if (!enabled) this else this.then(
    Modifier.onPointerEvent(PointerEventType.Press, PointerEventPass.Initial) { event ->
      val modifiers = event.keyboardModifiers
      if (modifiers.isShiftPressed || modifiers.isMetaPressed) {
        event.changes.forEach { it.consume() }
        if (modifiers.isShiftPressed) onRange() else onToggle()
      }
    }
  )

actual fun Modifier.desktopOnHovered(action: (Boolean) -> Unit): Modifier =
  this then Modifier
    .onPointerEvent(PointerEventType.Enter) { action(true) }
    .onPointerEvent(PointerEventType.Exit) { action(false) }
