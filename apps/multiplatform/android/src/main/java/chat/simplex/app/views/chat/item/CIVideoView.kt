package chat.simplex.app.views.chat.item

import android.graphics.Rect
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.runtime.*
import androidx.compose.ui.*
import androidx.compose.ui.platform.*
import androidx.compose.ui.unit.*
import androidx.compose.ui.viewinterop.AndroidView
import chat.simplex.app.views.helpers.*
import com.google.android.exoplayer2.ui.AspectRatioFrameLayout.RESIZE_MODE_FIXED_WIDTH
import com.google.android.exoplayer2.ui.StyledPlayerView


@Composable
fun PlayerView(player: VideoPlayer, width: Dp, onClick: () -> Unit, onLongClick: () -> Unit, stop: () -> Unit) {
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

@Composable fun LocalWindowWidth(): Dp {
  val view = LocalView.current
  val density = LocalDensity.current.density
  return remember {
    val rect = Rect()
    view.getWindowVisibleDisplayFrame(rect)
    (rect.width() / density).dp
  }
}
