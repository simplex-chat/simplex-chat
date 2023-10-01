package chat.simplex.common.views.chat.item

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.awt.SwingPanel
import androidx.compose.ui.graphics.*
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.unit.dp
import chat.simplex.common.platform.*
import chat.simplex.common.simplexWindowState
import chat.simplex.common.views.helpers.getBitmapFromByteArray
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.delay
import kotlin.math.max

@Composable
actual fun FullScreenImageView(modifier: Modifier, data: ByteArray, imageBitmap: ImageBitmap) {
  Image(
    getBitmapFromByteArray(data, false) ?: MR.images.decentralized.image.toComposeImageBitmap(),
    contentDescription = stringResource(MR.strings.image_descr),
    contentScale = ContentScale.Fit,
    modifier = modifier,
  )
}
@Composable
actual fun FullScreenVideoView(player: VideoPlayer, modifier: Modifier, close: () -> Unit) {
  // Workaround. Without changing size of the window the screen flashes a lot even if it's not being recomposed
  LaunchedEffect(Unit) {
    simplexWindowState.windowState.size = simplexWindowState.windowState.size.copy(width = simplexWindowState.windowState.size.width + 1.dp)
    delay(50)
    player.play(true)
    simplexWindowState.windowState.size = simplexWindowState.windowState.size.copy(width = simplexWindowState.windowState.size.width - 1.dp)
  }
  Box {
    Box(Modifier.fillMaxSize().padding(bottom = 50.dp)) {
      val factory = remember { { player.mediaPlayerComponent } }
      SwingPanel(
        background = Color.Transparent,
        modifier = Modifier,
        factory = factory
      )
    }
    Controls(player, close)
  }
}

@Composable
private fun BoxScope.Controls(player: VideoPlayer, close: () -> Unit) {
  val playing = remember(player) { player.videoPlaying }
  val progress = remember(player) { player.progress }
  val duration = remember(player) { player.duration }
  Row(Modifier.fillMaxWidth().align(Alignment.BottomCenter).height(50.dp)) {
    IconButton(onClick = { if (playing.value) player.player.pause() else player.play(true) },) {
      Icon(painterResource(if (playing.value) MR.images.ic_pause_filled else MR.images.ic_play_arrow_filled), null, Modifier.size(30.dp), tint = MaterialTheme.colors.primary)
    }
    Slider(
      value = progress.value.toFloat() / max(0.0001f, duration.value.toFloat()),
      onValueChange = { player.player.seekTo((it * duration.value).toInt()) },
      modifier = Modifier.fillMaxWidth().weight(1f)
    )
    IconButton(onClick = close,) {
      Icon(painterResource(MR.images.ic_close), null, Modifier.size(30.dp), tint = MaterialTheme.colors.primary)
    }
  }
}
