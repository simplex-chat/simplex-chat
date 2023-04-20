package chat.simplex.app.views.helpers

import androidx.compose.foundation.layout.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.onFocusChanged
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.views.newchat.ActionButton

sealed class AttachmentOption {
  object CameraPhoto: AttachmentOption()
  object GalleryImage: AttachmentOption()
  object GalleryVideo: AttachmentOption()
  object File: AttachmentOption()
}

@Composable
fun ChooseAttachmentView(
  attachmentOption: MutableState<AttachmentOption?>,
  hide: () -> Unit
) {
  Box(
    modifier = Modifier
      .fillMaxWidth()
      .wrapContentHeight()
      .onFocusChanged { focusState ->
        if (!focusState.hasFocus) hide()
      }
  ) {
    Row(
      Modifier
        .fillMaxWidth()
        .padding(horizontal = 8.dp, vertical = 30.dp),
      horizontalArrangement = Arrangement.SpaceEvenly
    ) {
      ActionButton(null, stringResource(R.string.use_camera_button), icon = painterResource(R.drawable.ic_camera_enhance), Color.White) {
        attachmentOption.value = AttachmentOption.CameraPhoto
        hide()
      }
      ActionButton(null, stringResource(R.string.gallery_image_button), icon = painterResource(R.drawable.ic_add_photo), Color.White) {
        attachmentOption.value = AttachmentOption.GalleryImage
        hide()
      }
      ActionButton(null, stringResource(R.string.gallery_video_button), icon = painterResource(R.drawable.ic_smart_display), Color.White) {
        attachmentOption.value = AttachmentOption.GalleryVideo
        hide()
      }
      ActionButton(null, stringResource(R.string.choose_file), icon = painterResource(R.drawable.ic_note_add), Color.White) {
        attachmentOption.value = AttachmentOption.File
        hide()
      }
    }
  }
}
