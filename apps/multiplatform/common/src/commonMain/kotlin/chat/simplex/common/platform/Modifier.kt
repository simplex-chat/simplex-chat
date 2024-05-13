package chat.simplex.common.platform

import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.painter.Painter
import java.io.File

expect fun Modifier.navigationBarsWithImePadding(): Modifier

@Composable
expect fun ProvideWindowInsets(
  consumeWindowInsets: Boolean = true,
  windowInsetsAnimationsEnabled: Boolean = true,
  content: @Composable () -> Unit
)

@Composable
expect fun Modifier.desktopOnExternalDrag(
  enabled: Boolean = true,
  onFiles: (List<File>) -> Unit = {},
  onImage: (Painter) -> Unit = {},
  onText: (String) -> Unit = {}
): Modifier

expect fun Modifier.onRightClick(action: () -> Unit): Modifier

expect fun Modifier.desktopPointerHoverIconHand(): Modifier
