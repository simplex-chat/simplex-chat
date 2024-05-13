package chat.simplex.common.views.helpers

import androidx.compose.foundation.layout.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.onFocusChanged
import androidx.compose.ui.graphics.ImageBitmap
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.common.platform.rememberFileChooserLauncher
import chat.simplex.res.MR
import chat.simplex.common.views.newchat.ActionButton
import java.net.URI

@Composable
actual fun GetImageBottomSheet(
  imageBitmap: MutableState<URI?>,
  onImageChange: (ImageBitmap) -> Unit,
  hideBottomSheet: () -> Unit
) {
  Box(
    modifier = Modifier
      .fillMaxWidth()
      .wrapContentHeight()
      .onFocusChanged { focusState ->
        if (!focusState.hasFocus) hideBottomSheet()
      }
  ) {
    Row(
      Modifier
        .fillMaxWidth()
        .padding(horizontal = 8.dp, vertical = 30.dp),
      horizontalArrangement = Arrangement.SpaceEvenly
    ) {
      val processPickedImage = { uri: URI? ->
        if (uri != null) {
          val bitmap = getBitmapFromUri(uri)
          if (bitmap != null) {
            imageBitmap.value = uri
            onImageChange(bitmap)
          }
          hideBottomSheet()
        }
      }
      val pickImageLauncher = rememberFileChooserLauncher(true, null, processPickedImage)
      ActionButton(null, stringResource(MR.strings.from_gallery_button), icon = painterResource(MR.images.ic_image)) {
        withLongRunningApi { pickImageLauncher.launch("image/*") }
      }
    }
  }
}
