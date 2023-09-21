package chat.simplex.common.views.chat.item

import androidx.compose.runtime.*
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.unit.Dp
import chat.simplex.common.platform.VideoPlayer

@Composable
actual fun PlayerView(player: VideoPlayer, width: Dp, onClick: () -> Unit, onLongClick: () -> Unit, stop: () -> Unit) {}

@Composable
actual fun LocalWindowWidth(): Dp {
  return with(LocalDensity.current) { (java.awt.Window.getWindows().find { it.isActive }?.width ?: 0).toDp() }
  /*val density = LocalDensity.current
  var width by remember { mutableStateOf(with(density) { (java.awt.Window.getWindows().find { it.isActive }?.width ?: 0).toDp() }) }
  SideEffect {
    if (width != with(density) { (java.awt.Window.getWindows().find { it.isActive }?.width ?: 0).toDp() })
      width = with(density) { (java.awt.Window.getWindows().find { it.isActive }?.width ?: 0).toDp() }
  }
  return width.also { println("LALAL $it") }*/
}
