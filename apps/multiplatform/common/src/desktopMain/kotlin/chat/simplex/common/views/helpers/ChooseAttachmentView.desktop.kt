package chat.simplex.common.views.helpers

import androidx.compose.foundation.layout.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState
import androidx.compose.ui.Modifier
import chat.simplex.common.views.newchat.ActionButton
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

@Composable
actual fun ChooseAttachmentButtons(attachmentOption: MutableState<AttachmentOption?>, hide: () -> Unit) {
  ActionButton(Modifier.fillMaxWidth(0.33f), null, stringResource(MR.strings.gallery_image_button), icon = painterResource(MR.images.ic_add_photo)) {
    attachmentOption.value = AttachmentOption.GalleryImage
    hide()
  }
  ActionButton(Modifier.fillMaxWidth(0.5f), null, stringResource(MR.strings.gallery_video_button), icon = painterResource(MR.images.ic_smart_display)) {
    attachmentOption.value = AttachmentOption.GalleryVideo
    hide()
  }
  ActionButton(Modifier.fillMaxWidth(1f), null, stringResource(MR.strings.choose_file), icon = painterResource(MR.images.ic_note_add)) {
    attachmentOption.value = AttachmentOption.File
    hide()
  }
}
