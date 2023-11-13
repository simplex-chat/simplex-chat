package chat.simplex.common.views.chat.item

import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.painter.BitmapPainter
import androidx.compose.ui.graphics.painter.Painter
import chat.simplex.common.model.CIFile
import chat.simplex.common.model.RemoteFile
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.ModalManager
import chat.simplex.common.views.helpers.withBGApi
import java.io.File
import java.net.URI

@Composable
actual fun SimpleAndAnimatedImageView(
  data: ByteArray,
  imageBitmap: ImageBitmap,
  file: CIFile?,
  imageProvider: () -> ImageGalleryProvider,
  ImageView: @Composable (painter: Painter, onClick: () -> Unit) -> Unit
) {
  // LALAL make it animated too
  LaunchedEffect(Unit) {
    val rh = chatModel.currentRemoteHost.value
    val user = chatModel.currentUser.value
    Log.e(TAG, "LaunchEffect")
    if (rh == null || user == null || file?.fileSource == null) return@LaunchedEffect
    Log.e(TAG, "LOADING?")
    val f = getLoadedFilePath(file, checkExists = false)
    Log.e(TAG, "LOADING path $f")
    if (f == null || File(f).exists()) return@LaunchedEffect
    withBGApi {
      val rf = RemoteFile(
        userId = user.userId,
        fileId = file.fileId,
        sent = file.fileStatus.sent,
        fileSource = file.fileSource
      )
      Log.e(TAG, "LOADING in withBGApi")
      chatModel.controller.getRemoteFile(rh.remoteHostId, rf)
      Log.e(TAG, "LOADED")
    }
  }
  ImageView(imageBitmap.toAwtImage().toPainter()) {
    if (getLoadedFilePath(file) != null) {
      ModalManager.fullscreen.showCustomModal(animated = false) { close ->
        ImageFullScreenView(imageProvider, close)
      }
    }
  }
}
