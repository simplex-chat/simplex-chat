package chat.simplex.common.views.chat

import androidx.compose.runtime.*
import chat.simplex.common.views.helpers.AttachmentOption
import java.net.URI

@Composable
actual fun AttachmentSelection(
  composeState: MutableState<ComposeState>,
  attachmentOption: MutableState<AttachmentOption?>,
  processPickedFile: (URI?, String?) -> Unit,
  processPickedMedia: (List<URI>, String?) -> Unit
) {
  LaunchedEffect(attachmentOption.value) {
    when (attachmentOption.value) {
      AttachmentOption.CameraPhoto -> {}
      AttachmentOption.GalleryImage -> {}
      AttachmentOption.GalleryVideo -> {}
      AttachmentOption.File -> {}
      else -> {}
    }
    attachmentOption.value = null
  }
}