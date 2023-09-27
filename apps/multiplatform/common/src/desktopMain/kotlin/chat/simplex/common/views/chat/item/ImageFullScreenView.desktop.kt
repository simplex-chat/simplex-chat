package chat.simplex.common.views.chat.item

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.*
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.unit.dp
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.getBitmapFromByteArray
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import org.jetbrains.compose.videoplayer.SkiaBitmapVideoSurface
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
  Box {
    Box(Modifier.fillMaxSize().padding(bottom = 50.dp)) {
      SurfaceFromPlayer(player, modifier)
      IconButton(onClick = close, Modifier.padding(top = 5.dp)) {
        Icon(painterResource(MR.images.ic_arrow_back_ios_new), null, Modifier.size(30.dp), tint = MaterialTheme.colors.primary)
      }
    }
    Controls(player)
  }
}

@Composable
fun BoxScope.SurfaceFromPlayer(player: VideoPlayer, modifier: Modifier) {
  val surface = remember {
    SkiaBitmapVideoSurface().also {
      player.player.videoSurface().set(it)
    }
  }
  surface.bitmap.value?.let { bitmap ->
    Image(
      bitmap,
      modifier = modifier.align(Alignment.Center),
      contentDescription = null,
      contentScale = ContentScale.Fit,
      alignment = Alignment.Center,
    )
  }
}

@Composable
private fun BoxScope.Controls(player: VideoPlayer) {
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
      modifier = Modifier.fillMaxWidth()
    )
  }
}
