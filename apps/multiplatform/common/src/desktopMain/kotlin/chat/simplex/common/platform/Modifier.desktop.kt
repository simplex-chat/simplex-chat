package chat.simplex.common.platform

import androidx.compose.foundation.contextMenuOpenDetector
import androidx.compose.foundation.draganddrop.dragAndDropTarget
import androidx.compose.runtime.Composable
import androidx.compose.runtime.remember
import androidx.compose.ui.*
import androidx.compose.ui.draganddrop.*
import androidx.compose.ui.draganddrop.DragData
import androidx.compose.ui.input.pointer.*
import chat.simplex.common.simplexWindowState
import java.awt.Component
import java.awt.Container
import java.awt.Cursor
import java.awt.Image
import java.awt.Window
import java.awt.datatransfer.DataFlavor
import java.awt.datatransfer.Transferable
import java.awt.image.BufferedImage
import java.io.File
import java.lang.ref.WeakReference
import java.net.URI
import javax.swing.SwingUtilities

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

// hot path: canvas lookup (and its failure) cached per window, weakly to not retain a recreated window
private var cachedSkiaCanvas: WeakReference<Component>? = null
private var skiaCanvasMissingIn: WeakReference<Window>? = null

actual fun desktopSetHoverCursor(icon: PointerIcon) {
  // clickable message text only exists in the main window
  val window = simplexWindowState.window ?: return
  val type = when (icon) {
    PointerIcon.Hand -> Cursor.HAND_CURSOR
    PointerIcon.Text -> Cursor.TEXT_CURSOR
    else -> Cursor.DEFAULT_CURSOR
  }
  var canvas = cachedSkiaCanvas?.get()
  if (canvas == null || SwingUtilities.getWindowAncestor(canvas) !== window) {
    if (skiaCanvasMissingIn?.get() === window) return
    canvas = findSkiaCanvas(window)
    if (canvas == null) {
      Log.w(TAG, "desktopSetHoverCursor: Skia canvas not found, hover cursor workaround disabled")
      skiaCanvasMissingIn = WeakReference(window)
      return
    }
    cachedSkiaCanvas = WeakReference(canvas)
  }
  if (canvas.cursor.type != type) canvas.cursor = Cursor.getPredefinedCursor(type)
}

// the same component Compose writes pointer icons to (ComposeSceneMediator.setPointerIcon)
private fun findSkiaCanvas(c: Component): Component? = when {
  c.javaClass.name.contains("Skia") -> c
  c is Container -> c.components.firstNotNullOfOrNull { findSkiaCanvas(it) }
  else -> null
}

actual fun Modifier.desktopOnHovered(action: (Boolean) -> Unit): Modifier =
  this then Modifier
    .onPointerEvent(PointerEventType.Enter) { action(true) }
    .onPointerEvent(PointerEventType.Exit) { action(false) }
