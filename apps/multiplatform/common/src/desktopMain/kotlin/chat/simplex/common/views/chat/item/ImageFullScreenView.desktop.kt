package chat.simplex.common.views.chat.item

import androidx.compose.foundation.Image
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.*
import androidx.compose.ui.layout.ContentScale
import chat.simplex.common.platform.VideoPlayer
import chat.simplex.common.views.helpers.getBitmapFromByteArray
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.stringResource

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
actual fun FullScreenVideoView(player: VideoPlayer, modifier: Modifier) {

}
