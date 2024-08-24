package chat.simplex.common.platform

import androidx.compose.foundation.contextMenuOpenDetector
import androidx.compose.runtime.Composable
import androidx.compose.ui.*
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.input.pointer.*
import java.io.File
import java.net.URI

actual fun Modifier.navigationBarsWithImePadding(): Modifier = this

@Composable
actual fun ProvideWindowInsets(
  consumeWindowInsets: Boolean,
  windowInsetsAnimationsEnabled: Boolean,
  content: @Composable () -> Unit
) {
  content()
}

@Composable
actual fun Modifier.desktopOnExternalDrag(
  enabled: Boolean,
  onFiles: (List<File>) -> Unit,
  onImage: (Painter) -> Unit,
  onText: (String) -> Unit
): Modifier =
onExternalDrag(enabled) {
  when(val data = it.dragData) {
    // data.readFiles() returns filePath in URI format (where spaces replaces with %20). But it's an error-prone idea to work later
    // with such format when everywhere we use absolutePath in File() format
    is DragData.FilesList -> onFiles(data.readFiles().map { URI.create(it).toFile() })
    is DragData.Image -> onImage(data.readImage())
    is DragData.Text -> onText(data.readText())
  }
}

actual fun Modifier.onRightClick(action: () -> Unit): Modifier = contextMenuOpenDetector { action() }

actual fun Modifier.desktopPointerHoverIconHand(): Modifier = Modifier.pointerHoverIcon(PointerIcon.Hand)

actual fun Modifier.desktopOnHovered(action: (Boolean) -> Unit): Modifier =
  this then Modifier
    .onPointerEvent(PointerEventType.Enter) { action(true) }
    .onPointerEvent(PointerEventType.Exit) { action(false) }
