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
  object TakePhoto: AttachmentOption()
  object PickImage: AttachmentOption()
  object PickFile: AttachmentOption()
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
        attachmentOption.value = AttachmentOption.TakePhoto
        hide()
      }
      ActionButton(null, stringResource(R.string.from_gallery_button), icon = Icons.Outlined.Collections) {
        attachmentOption.value = AttachmentOption.PickImage
        hide()
      }
      ActionButton(null, stringResource(R.string.choose_file), icon = Icons.Outlined.InsertDriveFile) {
        attachmentOption.value = AttachmentOption.PickFile
        hide()
      }
    }
  }
}
