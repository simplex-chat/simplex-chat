package chat.simplex.common.views.chat

import androidx.compose.runtime.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.*
import java.net.URI

@Composable
actual fun AttachmentSelection(
  composeState: MutableState<ComposeState>,
  attachmentOption: MutableState<AttachmentOption?>,
  processPickedFile: (URI?, String?) -> Unit,
  processPickedMedia: (List<URI>, String?) -> Unit
) {
  val imageLauncher = rememberFileChooserMultipleLauncher {
    processPickedMedia(it, null)
  }
  val videoLauncher = rememberFileChooserMultipleLauncher {
    processPickedMedia(it, null)
  }
  val filesLauncher = rememberFileChooserLauncher(true) {
    if (it != null) processPickedFile(it, null)
  }
  LaunchedEffect(attachmentOption.value) {
    when (attachmentOption.value) {
      AttachmentOption.CameraPhoto -> {}
      AttachmentOption.GalleryImage -> {
        imageLauncher.launch("image/*")
      }
      AttachmentOption.GalleryVideo -> {
        videoLauncher.launch("video/*")
      }
      AttachmentOption.File -> {
        filesLauncher.launch("*/*")
      }
      else -> {}
    }
    attachmentOption.value = null
  }
}
