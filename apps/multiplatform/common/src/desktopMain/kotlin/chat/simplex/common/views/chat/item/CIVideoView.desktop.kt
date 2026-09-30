package chat.simplex.common.views.chat.item

import androidx.compose.foundation.combinedClickable
import androidx.compose.foundation.layout.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.platform.LocalWindowInfo
import androidx.compose.ui.unit.Dp
import chat.simplex.common.platform.*
import chat.simplex.common.simplexWindowState
import java.awt.Window

@Composable
actual fun PlayerView(player: VideoPlayer, width: Dp, onClick: () -> Unit, onLongClick: () -> Unit, stop: () -> Unit) {
  Box {
    // The preview this replaces while playing is drawn with FillWidth, so a video smaller than the
    // item width has to grow the same way here - Fit would leave it at its own size in the middle
    SurfaceFromPlayer(player,
      Modifier
        .width(width)
        .combinedClickable(
          onLongClick = onLongClick,
          onClick = { if (player.player.isPlaying) stop() else onClick() }
        )
        .onRightClick(onLongClick),
      contentScale = ContentScale.FillWidth
    )
  }
}

/*
* This function doesn't take into account multi-window environment. In case more windows will be used, modify the code
* */
@Composable
actual fun LocalWindowWidth(): Dp = with(LocalDensity.current) {
  val windows = java.awt.Window.getWindows()
  if (windows.size == 1) {
    (windows.getOrNull(0)?.width ?: 0).toDp()
  } else {
    simplexWindowState.windowState.size.width
  }
}

@Composable
actual fun LocalWindowHeight(): Dp = with(LocalDensity.current) { LocalWindowInfo.current.containerSize.height.toDp() }
