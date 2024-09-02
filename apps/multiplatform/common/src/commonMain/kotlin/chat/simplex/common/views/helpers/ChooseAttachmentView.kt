package chat.simplex.common.views.helpers

import androidx.compose.foundation.layout.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.onFocusChanged
import androidx.compose.ui.unit.dp

sealed class AttachmentOption {
  object CameraPhoto: AttachmentOption()
  object GalleryImage: AttachmentOption()
  object GalleryVideo: AttachmentOption()
  object File: AttachmentOption()
}

@Composable
fun ChooseAttachmentView(attachmentOption: MutableState<AttachmentOption?>, hide: () -> Unit) {
  Box(
    modifier = Modifier
      .fillMaxWidth()
      .navigationBarsPadding()
      .imePadding()
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
      ChooseAttachmentButtons(attachmentOption, hide)
    }
  }
}

@Composable
expect fun ChooseAttachmentButtons(attachmentOption: MutableState<AttachmentOption?>, hide: () -> Unit)
