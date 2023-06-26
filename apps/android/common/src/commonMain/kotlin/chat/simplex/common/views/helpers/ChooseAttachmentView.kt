package chat.simplex.common.views.helpers

import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.onFocusChanged
import androidx.compose.ui.graphics.Color
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import com.icerockdev.library.MR
import chat.simplex.common.views.newchat.ActionButton

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
      ActionButton(Modifier.fillMaxWidth(0.25f), null, stringResource(MR.strings.use_camera_button), icon = painterResource(MR.images.ic_camera_enhance)) {
        attachmentOption.value = AttachmentOption.CameraPhoto
        hide()
      }
      ActionButton(Modifier.fillMaxWidth(0.33f), null, stringResource(MR.strings.gallery_image_button), icon = painterResource(MR.images.ic_add_photo)) {
        attachmentOption.value = AttachmentOption.GalleryImage
        hide()
      }
      ActionButton(Modifier.fillMaxWidth(0.50f), null, stringResource(MR.strings.gallery_video_button), icon = painterResource(MR.images.ic_smart_display)) {
        attachmentOption.value = AttachmentOption.GalleryVideo
        hide()
      }
      ActionButton(Modifier.fillMaxWidth(1f), null, stringResource(MR.strings.choose_file), icon = painterResource(MR.images.ic_note_add)) {
        attachmentOption.value = AttachmentOption.File
        hide()
      }
    }
  }
}
