package chat.simplex.common.views.chat.item

import androidx.compose.foundation.Image
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.painter.BitmapPainter
import androidx.compose.ui.layout.ContentScale
import chat.simplex.common.platform.VideoPlayer
import chat.simplex.common.views.helpers.getBitmapFromUri
import com.icerockdev.library.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import java.net.URI

@Composable
actual fun FullScreenImageView(modifier: Modifier, uri: URI, imageBitmap: ImageBitmap) {
  Image(
    getBitmapFromUri(uri, false) ?: MR.images.decentralized.image.toComposeImageBitmap(),
    contentDescription = stringResource(MR.strings.image_descr),
    contentScale = ContentScale.Fit,
    modifier = modifier,
  )
}
@Composable
actual fun FullScreenVideoView(player: VideoPlayer, modifier: Modifier) {

}