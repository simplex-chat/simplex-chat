package chat.simplex.common.platform

import androidx.compose.runtime.Composable
import androidx.compose.ui.*
import androidx.compose.ui.graphics.painter.Painter

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
  onFiles: (List<String>) -> Unit,
  onImage: (Painter) -> Unit,
  onText: (String) -> Unit
): Modifier =
onExternalDrag(enabled) {
  when(val data = it.dragData) {
    is DragData.FilesList -> onFiles(data.readFiles())
    is DragData.Image -> onImage(data.readImage())
    is DragData.Text -> onText(data.readText())
  }
}
