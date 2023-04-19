package chat.simplex.app.views.helpers

import androidx.compose.foundation.layout.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.onFocusChanged
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
      ActionButton(null, stringResource(R.string.use_camera_button), icon = Icons.Outlined.PhotoCamera) {
        attachmentOption.value = AttachmentOption.CameraPhoto
        hide()
      }
      ActionButton(null, stringResource(R.string.gallery_image_button), icon = Icons.Outlined.Image) {
        attachmentOption.value = AttachmentOption.GalleryImage
        hide()
      }
      ActionButton(null, stringResource(R.string.gallery_video_button), icon = Icons.Outlined.SmartDisplay) {
        attachmentOption.value = AttachmentOption.GalleryVideo
        hide()
      }
      ActionButton(null, stringResource(R.string.choose_file), icon = Icons.Outlined.InsertDriveFile) {
        attachmentOption.value = AttachmentOption.File
        hide()
      }
    }
  }
}
