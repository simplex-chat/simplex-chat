package chat.simplex.common.views.chat.item

import androidx.compose.foundation.combinedClickable
import androidx.compose.foundation.layout.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.unit.Dp
import chat.simplex.common.platform.*

@Composable
actual fun PlayerView(player: VideoPlayer, width: Dp, onClick: () -> Unit, onLongClick: () -> Unit, stop: () -> Unit) {
  Box {
    SurfaceFromPlayer(player,
      Modifier
        .width(width)
        .combinedClickable(
          onLongClick = onLongClick,
          onClick = { if (player.player.isPlaying) stop() else onClick() }
        )
        .onRightClick(onLongClick)
    )
  }
}

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
