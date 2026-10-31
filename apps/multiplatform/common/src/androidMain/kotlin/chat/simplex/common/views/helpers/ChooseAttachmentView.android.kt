package chat.simplex.common.views.helpers

import androidx.compose.foundation.layout.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState
import androidx.compose.ui.Modifier
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.ui
import chat.simplex.common.views.newchat.ActionButton
import chat.simplex.res.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

@Composable
actual fun ChooseAttachmentButtons(attachmentOption: MutableState<AttachmentOption?>, hide: () -> Unit) {
  ActionButton(Modifier.fillMaxWidth(0.25f).ui(Ui.Chat.Attach("camera")), null, stringResource(MR.strings.use_camera_button), icon = painterResource(MR.images.ic_camera_enhance)) {
    attachmentOption.value = AttachmentOption.CameraPhoto
    hide()
  }
  ActionButton(Modifier.fillMaxWidth(0.33f).ui(Ui.Chat.Attach("image")), null, stringResource(MR.strings.gallery_image_button), icon = painterResource(MR.images.ic_add_photo)) {
    attachmentOption.value = AttachmentOption.GalleryImage
    hide()
  }
  ActionButton(Modifier.fillMaxWidth(0.50f).ui(Ui.Chat.Attach("video")), null, stringResource(MR.strings.gallery_video_button), icon = painterResource(MR.images.ic_smart_display)) {
    attachmentOption.value = AttachmentOption.GalleryVideo
    hide()
  }
  ActionButton(Modifier.fillMaxWidth(1f).ui(Ui.Chat.Attach("file")), null, stringResource(MR.strings.choose_file), icon = painterResource(MR.images.ic_note_add)) {
    attachmentOption.value = AttachmentOption.File
    hide()
  }
}
