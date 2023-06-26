package chat.simplex.common.views.chat.item

import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState
import chat.simplex.common.model.ChatItem
import chat.simplex.common.model.MsgContent
import chat.simplex.common.platform.copyFileToFile
import chat.simplex.common.platform.rememberFileChooserLauncher
import chat.simplex.common.views.helpers.*
import com.icerockdev.library.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import java.io.File

@Composable
actual fun SaveContentItemAction(cItem: ChatItem, showMenu: MutableState<Boolean>) {
  val saveFileLauncher = rememberFileChooserLauncher(false) { to ->
    if (cItem.file?.fileName != null && to != null) {
      val file = File(getAppFileUri(cItem.file.fileName))
      copyFileToFile(file, to) {}
    }
  }
  ItemAction(stringResource(MR.strings.save_verb), painterResource(MR.images.ic_download), onClick = {
    when (cItem.content.msgContent) {
      is MsgContent.MCImage -> saveImage(getAppFileUri(cItem.file?.fileName ?: return@ItemAction))
      is MsgContent.MCFile, is MsgContent.MCVoice, is MsgContent.MCVideo -> withApi { saveFileLauncher.launch(cItem.file?.fileName ?: "") }
      else -> {}
    }
    showMenu.value = false
  })
}