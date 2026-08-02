package chat.simplex.common.platform

import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.painter.Painter
import java.io.File

@Composable
actual fun Modifier.desktopOnExternalDrag(
  enabled: Boolean,
  onFiles: (List<File>) -> Unit,
  onImage: (File) -> Unit,
  onText: (String) -> Unit,
  onDragging: (Boolean) -> Unit,
): Modifier = this

actual fun Modifier.onRightClick(action: () -> Unit): Modifier = this

actual fun Modifier.desktopPointerHoverIconHand(): Modifier = this

actual fun Modifier.desktopPointerHoverIconResize(): Modifier = this

actual val macOSWindowVibrancyAvailable: Boolean = false

actual fun openSystemNotificationSettings() {}

@Composable
actual fun Modifier.desktopMessageSelection(enabled: Boolean, onToggle: () -> Unit, onRange: () -> Unit): Modifier = this

actual fun Modifier.desktopOnHovered(action: (Boolean) -> Unit): Modifier = Modifier
