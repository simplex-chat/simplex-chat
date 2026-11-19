package chat.simplex.common.platform

import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.autofill.ContentType
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.text.TextLayoutResult
import java.io.File

@Composable
actual fun Modifier.desktopOnExternalDrag(
  enabled: Boolean,
  onFiles: (List<File>) -> Unit,
  onImage: (File) -> Unit,
  onText: (String) -> Unit
): Modifier = this

actual fun Modifier.onRightClick(action: () -> Unit): Modifier = this

actual fun Modifier.desktopPointerHoverIconHand(): Modifier = this

actual fun Modifier.desktopOnHovered(action: (Boolean) -> Unit): Modifier = Modifier

@Composable
actual fun Modifier.desktopDrawnBaselines(textLayout: () -> TextLayoutResult?): Modifier = this

actual fun desktopLineHeightPx(fontSizePx: Float, lineHeightPx: Float): Float = lineHeightPx

actual fun newPasswordContentType(): ContentType = ContentType.NewPassword + ContentType.Password
