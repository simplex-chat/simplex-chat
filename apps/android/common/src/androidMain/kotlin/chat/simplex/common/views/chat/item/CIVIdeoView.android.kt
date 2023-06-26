package chat.simplex.common.views.chat.item

import android.graphics.Rect
import androidx.compose.foundation.combinedClickable
import androidx.compose.foundation.layout.width
import androidx.compose.runtime.Composable
import androidx.compose.runtime.remember
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.platform.LocalView
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import androidx.compose.ui.viewinterop.AndroidView
import chat.simplex.common.platform.VideoPlayer
import com.google.android.exoplayer2.ui.AspectRatioFrameLayout.RESIZE_MODE_FIXED_WIDTH
import com.google.android.exoplayer2.ui.StyledPlayerView

@Composable
actual fun PlayerView(player: VideoPlayer, width: Dp, onClick: () -> Unit, onLongClick: () -> Unit, stop: () -> Unit) {
  AndroidView(
    factory = { ctx ->
      StyledPlayerView(ctx).apply {
        useController = false
        resizeMode = RESIZE_MODE_FIXED_WIDTH
        this.player = player.player
      }
    },
    Modifier
      .width(width)
      .combinedClickable(
        onLongClick = onLongClick,
        onClick = { if (player.player.playWhenReady) stop() else onClick() }
      )
  )
}

@Composable
actual fun LocalWindowWidth(): Dp {
  val view = LocalView.current
  val density = LocalDensity.current.density
  return remember {
    val rect = Rect()
    view.getWindowVisibleDisplayFrame(rect)
    (rect.width() / density).dp
  }
}