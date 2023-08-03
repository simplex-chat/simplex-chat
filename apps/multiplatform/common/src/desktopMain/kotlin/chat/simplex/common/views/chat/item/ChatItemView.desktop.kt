package chat.simplex.common.views.chat.item

import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState
import chat.simplex.common.model.ChatItem
import chat.simplex.common.model.MsgContent
import chat.simplex.common.platform.FileChooserLauncher
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

@Composable
actual fun SaveContentItemAction(cItem: ChatItem, saveFileLauncher: FileChooserLauncher, showMenu: MutableState<Boolean>) {
  ItemAction(stringResource(MR.strings.save_verb), painterResource(MR.images.ic_download), onClick = {
    when (cItem.content.msgContent) {
      is MsgContent.MCImage, is MsgContent.MCFile, is MsgContent.MCVoice, is MsgContent.MCVideo -> withApi { saveFileLauncher.launch(cItem.file?.fileName ?: "") }
      else -> {}
    }
    showMenu.value = false
  })
}
